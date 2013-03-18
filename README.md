HAVCR
=====

HAVCR is a testing tool for recording and playing back HTTP network transactions.
It is inspired by [VCR][] library from Ruby
and its records are compatible with VCR "cassettes".


Abstract
--------

Your networking code should be arranged such as to use [Control.Proxy][controlproxy]
(see also "Refactoring HTTP Calls" below).
When tests are running, HTTP requests are checked against prerecorded cassettes.
If the recorded request exists, its corresponding recorded response is sent back
to the application, without hitting the real network. If the recorded request is not
found, it is sent to the real network and, together with the response that is received,
recorded for the future reuse.


Refactoring HTTP Calls
----------------------

(Copied from [Network.HTTP To Control.Proxy Who Am The Only One][htohe]).

Provided that you use `simpleHTTP` function from `Network.HTTP` library,
here’s what you need to do to make it “testable”:

Your original function:

    getResponse :: HStream ty => Request ty -> IO (Result (Response ty))
    getResponse req = simpleHTTP req

should be modified to send requests via a Proxy:

    getResponse :: Proxy p => FilePath -> () -> Session p IO (Result (Response String))
    getResponse req = mockedServer "cassette.yml" >-> httpClient req

    httpClient :: forall ty p r. (HStream ty, IsString ty, Proxy p) =>
                  Request String -> () -> Client p (Request String) (Result (Response ty)) IO (Result (Response ty))
    httpClient req () = runIdentityP $ do request req

Don’t forget to import `Network.HAVCR` which provides `mockedServer`.


Authors and Contributors
------------------------

[Slava Kravchenko](https://github.com/cordawyn)


Thanks
------

[Gabriel Gonzalez](http://stackoverflow.com/users/1026598/gabriel-gonzalez)


[VCR]: https://github.com/myronmarston/vcr
[controlproxy]: http://hackage.haskell.org/packages/archive/pipes/latest/doc/html/Control-Proxy.html
[htohe]: http://sophiornithidae.tumblr.com/post/45659241983/network-http-to-control-proxy-who-am-the-only-one
