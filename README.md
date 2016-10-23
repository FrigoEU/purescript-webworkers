## Use Web Workers in PureScript

The WebWorker module just declares the usual JavaScript WebWorker interface enriched with PureScript type signatures. Note the IsWW and OwnsWW effects: These will "bubble up" to the top of your program and help you to seperate the WebWorker from the UI thread code, very handy!

The WebWorker.Channel module exposes an abstraction called Channel that enables typesafe communication between UI Thread and WebWorkers. Just make a Channel like this:

```
message1Channel :: Channel Int
message1Channel = Channel "message1Channel!!" 
-- The "message1Channel!!" string needs to unique. 
-- It's used to distinguish between different channels
```

Notice the phantom type Int in the above. This makes sure that the rest of the functions in WebWorker.Channel only allow sending Int's into it, and listening to this channel only accepts a callback that starts from an Int.

### Run tests

```
npm run buildtestmain && npm run buildtestworker
host ./static folder
go to ./static/index and check console
```
