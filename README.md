HAVCR
=====

HAVCR is a testing tool for recording and playing back HTTP network transactions.
It is inspired by [VCR][] library from Ruby
and its records are compatible with VCR "cassettes".


Abstract
--------

Your networking code should be arranged such as to use Control.Proxy
(see "Refactoring HTTP Calls" below).
When tests are running, HTTP requests are checked against prerecorded cassettes.
If the recorded request exists, its corresponding recorded response is sent back
to the application, without hitting the real network. If the recorded request is not
found, it is sent to the real network and, together with the response that is received,
recorded for the future reuse.


Refactoring HTTP Calls
----------------------

TODO (see [Haskell: Testing web APIs][stack1]).


Authors and Contributors
------------------------

[Slava Kravchenko](https://github.com/cordawyn)


Thanks
------

[Gabriel Gonzalez](http://stackoverflow.com/users/1026598/gabriel-gonzalez)


[VCR]: https://github.com/myronmarston/vcr
[stack1]: http://stackoverflow.com/questions/12424928/haskell-testing-web-apis
