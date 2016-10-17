## Zero Downtime Deployment with Warp

Using `warp` it is easy to perform zero downtime deployment with
`SO_REUSEPORT`. This literate Haskell file creates a server for zero downtime deploys and the repo has utilities that restart it without failed requests.

## Outline
- [`SO_REUSEPORT`](#so_reuseport)
- [Start Warp with `SO_REUSEPORT`](#start)
- [Reloading](#reloading)
- [Performance](#performance)
- [Design Analysis](#design)
- [Thanks](#thanks)

## `SO_REUSEPORT`

`SO_REUSEPORT` is an extension on newer versions of Linux and BSD (avoid OSX) that allows multiple sockets to bind to the same port. Additionally, Linux will load balance connections between sockets.

There is a downside to `SO_REUSEPORT`. When the number of sockets bound to a
port changes, there is the possibility that packets for a single TCP connection will get routed to two different sockets. This will lead to a failed requests. The likelihood is very low, but to prevent against this, we use a technique developed by Yelp.

#### Boring Haskell Import Statements

```haskell
import Control.Exception
import Data.ByteString.Lazy.Char8 (pack)
import Network.HTTP.Types
import Network.Socket
import Network.Wai
import Network.Wai.Handler.Warp
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Types
```

## <a name="start"> Start Warp with `SO_REUSEPORT`

First we need to create a socket with the `SO_REUSEPORT` flag. We repurpose some `streaming-commons` code.

This code opens a socket on localhost and crucially sets the `SO_REUSEPORT` flag.

```haskell
bindSocketReusePort :: PortNumber -> IO Socket
bindSocketReusePort p =
  bracketOnError (socket AF_INET Stream defaultProtocol) close $ \sock -> do
    mapM_ (uncurry $ setSocketOption sock)
          [ (NoDelay  , 1)
          , (ReuseAddr, 1)
          , (ReusePort, 1) -- <-- Here we add SO_REUSEPORT flag.
          ]
    bind sock $ SockAddrInet p $ tupleToHostAddress (127, 0, 0, 1)
    listen sock (max 2048 maxListenQueue)
    return sock
```

The server code takes advantage of two aspects of `warp`'s design.
  1. `warp` let's you start the server with a socket you have previously created.
  2. When the socket is closed `warp` gracefully shutdowns after it completes the current outstanding requests.

```haskell
main :: IO ()
main = do
  -- Before we shutdown an old version of the server, we need to run health
  -- checks on the new version. The minimal health check is ensuring the new
  -- server is responding to requests. We return the PID in every request to -- verify the server is running. We grab the PID on load here.
  CPid processId <- getProcessID
  -- We create our socket and setup a handler to close the socket on
  -- termination premature or otherwise.
  bracket
    (bindSocketReusePort 7000)
    close
    $ \sock -> do
       -- Before we start the server, we install a signal handler for
       -- `SIGTERM` to close the socket. This will start the graceful
       -- shutdown.
       installHandler sigTERM (CatchOnce $ close sock) Nothing
       -- Start the server with socket we created earlier.
       runSettingsSocket defaultSettings sock $ \_ responder ->
         -- Finally we create a single request for testing that the server is
         -- running by returning the PID.
         responder $ responseLBS status200 [] $ pack $ show processId
```

In a real server we would have many endpoints. We could either return the PID in a header or with a special health endpoint. However, our test server returns the PID regardless of the URL path parts or the HTTP verb.

## <a name="reloading"> Reloading

#### Setup

Before we can reload we need to setup a `plug` queuing discipline. This will let us pause `SYN` packets, e.g. new requests, temporarily while we bind or close a socket.

The following code was copied from the Yelp blog post. It is also in the repo in the `bin/setup-qdiscs` file. All operations require `sudo`.

```bash
# Set up the queuing discipline
tc qdisc add dev lo root handle 1: prio bands 4
tc qdisc add dev lo parent 1:1 handle 10: pfifo limit 1000
tc qdisc add dev lo parent 1:2 handle 20: pfifo limit 1000
tc qdisc add dev lo parent 1:3 handle 30: pfifo limit 1000

# Create a plug qdisc with 1 meg of buffer
nl-qdisc-add --dev=lo --parent=1:4 --id=40: plug --limit 1048576
# Release the plug
nl-qdisc-add --dev=lo --parent=1:4 --id=40: --update plug --release-indefinite

# Set up the filter, any packet marked with "1" will be
# directed to the plug
tc filter add dev lo protocol ip parent 1:0 prio 1 handle 1 fw classid 1:4

iptables -t mangle -I OUTPUT -p tcp -s 127.0.0.1 --syn -j MARK --set-mark 1
```

#### Reload

Reloading a new version in production requires a dance with your process supervisor. However the principle is similar even if the details are different.

  1. Stop additional `SYN` from being delivered using
     ```bash
     sudo nl-qdisc-add --dev=lo --parent=1:4 --id=40: \
                       --update plug --buffer
     ```
  1. Start a new version of the server and save the PID.
  1. Release the plug and let the `SYN` packets flow.
     ```bash
     sudo nl-qdisc-add --dev=lo --parent=1:4 --id=40: \
                       --update plug --release-indefinite
     ```
  1. Make requests to the health endpoint until the new PID is returned.
  1. Stop `SYN` packets again.
  1. Send `SIGTERM` to the other server processes so they will gracefully
     shutdown.
  1. Release the plug again.

An example for demonstrating this process can be found in `reload/Main.hs`. The `reload` app creates a new server and shutdowns all other instances. This is for demonstration purposes. In production you will want to integrate reloading with your process supervisor.

## <a name="performance"> Performance

This repo includes a Vagrant file for running a performance test. To run the test using the follow these steps.

```bash
$ vagrant up
$ vagrant ssh
$ cd /vagrant
$ stack build
$ bin/test
```

`bin/test` will:

  1. Build the project.
  1. Start the server.
  1. Run ab.
  1. Stop the server.
  1. Start the `reload` every 100 ms.
  1. Run ab.

Below are the times without constant reloading and with constant reloading. Crucially no connections are dropped and there are no requests failures.

#### Without Constant Reloading (Baseline)
- mean: 0.374 ms
- stddev: 0.2 ms
- 99%: 1 ms
- max: 67 ms

#### With Constant Reloading
- mean: 0.550 ms
- stddev: 1.9 ms
- 99%: 2 ms
- max: 276 ms

## <a name="design"> Design Analysis

#### Advantages
- It is relatively self contained
- Reloading is fast

#### Disadvantages
- The new version of the server is responding to requests **before health checks** are performed. If there is something wrong with the code, say it segfaults because the executable was corrupted, this will cause client impact.
- There is a small amount overhead because the requests are blocked during the reload.

### Immutable Alternative

One alternative would be to use an immutable blue/green deployment strategy, and utilize the load balancer to weigh in a new version.

#### Advantages
  * We weigh in the machine **after health checks**.
  * It doesn't necessarily have any overhead (depends on the load balancer YMMV)

#### Disadvantages
  * It can be slow to start up new VM's, which could prevent crucial fixes from
    being deployed.
  * Requires modifying the load balancers config, which is an easy way to cause
    catastrophic failure.
  * Arguably more work to setup.

### Future Work

An alternative design for utilizing `SO_REUSEPORT` is to create a parent process that keeps a pool of sockets and passes the file descriptions to the new children on reload. This is essentially the design that `nginx` has. The primary advantage is in the typical case we would not need to utilize the `plug` queuing discipline.

However, changing the number of child process will still be problematic but we could utilize the `plug` queueing discipline approach Yelp developed utilized here.

## <a name="thanks"> Thanks

I learned about `SO_REUSEPORT` from Imran Hamead, and the Yelp post was invaluable for preventing errors.
