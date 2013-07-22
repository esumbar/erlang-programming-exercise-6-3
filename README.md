# Exercise 6-3: A Supervisor Process

This is one possible solution to exercise 6-3 in the book _Erlang Programming_ by F. Cesarini and S. Thompson (O'Reilly, 2009). We are asked to expand the functionality of the basic supervisor described in the chapter in three ways.

1.	Handle two types of child processes: `permanent` and `transient`.
2.	Prevent the possibility of infinite restarts by throttling the restart rate.
3.	Allow child processes to be started and stopped after the supervisor has been started.

On my first attempt, I implemented the features in the order given. However, while testing the last of these, I encountered a problem.

I used the `add_two` module during development. When I caused `add_two:request/1` to crash by passing in an atom instead of an integer, the shell froze. I was expecting the supervisor to trap the exit, restart the child, and allow me to continue. What I was forgetting was that the shell client was waiting for a message from the crashed child that would never come. Thanks to Gleb Peregud on the `erlang-questions` list for pointing this out and suggesting the introduction of a monitor to deal with this issue.

Besides employing a monitor, I added a `stop/0` function to the `add_two` module to test the restart semantics for a `transient` child that terminates normally.

On my second attempt, I decided to start anew, but in the order 3-1-2.
