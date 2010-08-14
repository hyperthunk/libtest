## Libtest

I like writing tests but find it hard to write good *concurrent tests*. Tracking which Erlang processes have received which messages and in which order, swapping out generic OTP (gen_X) behaviours for stubs, the list goes on. And actually writing the assertions about these things is just as hard.

Libtest aims to *simplify* this stuff by providing

1. a bunch of services for capturing inter-process messages and context
2. support for stubbing out processes and OTP behaviours
3. an API based on Quickcheck's state machine model (statem) and integrated with [PropEr](http://github.com/manopapad/proper)
5. hamcrest matchers that make it simple to define your assertions

I'm also quite interested in McErlang integration, but that's probably quite a way off.

More documentation to follow. If you're brave enough to use (this is pre-alpha) then please submit bugs to the lighthouse project page (see below). Contributions welcome - you know what to do.

### Links

* Issues/Bugs: http://nebularis.lighthouseapp.com/projects/57524-libtest
* Iteration planning: http://www.pivotaltracker.com/projects/105408
