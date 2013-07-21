# Exercise 6-3: A Supervisor Process

This is one possible solution to exercise 6-3 in the book _Erlang Programming_ by F. Cesarini and S. Thompson (O'Reilly, 2009). We are asked to expand the functionality of the basic supervisor described in the chapter in three ways.

1.	Handle two types of child processes: `permanent` and `transient`.
2.	Prevent the possibility of infinite restarts by throttling the restart rate.
3.	Allow child processes to be started and stopped after the supervisor has been started.

On my first attempt, I implemented the features in the order given. However, while testing the last of these, I encountered a problem that I could not explain (then or now). Therefore, I decided to start anew, but in the order 3-1-2.

> Even after successfully completing the exercise, I still do not understand the problem. When a child function crashes (as when an atom is passed to the `add_two:request/1` function for example), it brings down the shell, which should get restarted automatically. However, because the shell and the supervisor are linked, the supervisor also tries to restart the shell, which is not in the child list. Adding a clause to the restart function to handle a child-not-found condition does not seem to solve the problem, so there is something else going on. In any case, the system hangs, and the only way to recover is to interrupt the shell. Upon restarting, the shell's pid is one greater than that of the restarted child's. 

> At one point, I thought it might have something to do with the fact that a child crash is propagated to the shell through the supervisor while also being handled directly by the shell that executed the function. Unfortunately, the same behaviour is observed when the supervisor is not linked to the shell.

During development, I used a modified version of the `add_two` module that included a `stop/0` function to test the restart semantics for a `transient` child that terminates normally.