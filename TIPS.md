## Tips & Tricks

### Booting up a node
```bash
  $ erl -name something@your.ip.goes.here -setcookie 'the-cookie'
```

### Starting the game
```erlang
  1> net_adm:ping('gold_fever@server.ip.goes.here').
```

### Keeping your progress safe
Since you'll most likely need to restart multiple times, I recommend creating a file (e.g. `solver.erl`).
Then, add a function (e.g. `run/0`) to it so that each time you need to restart, you can do…
```erlang
  1> c(solver).
  {ok, solver}
  2> solver:run().
  …
```

### Erlang tricks
#### RTFM
Check [erldocs](http://erldocs.com).

#### Remotely check process properties
```erlang
  rpc:pinfo(Pid, something).
```

#### Sending messages to FSMs
```erlang
  gen_fsm:sync_send_event(Pid, Msg).
  gen_fsm:sync_send_all_state_event(Pid, Msg).
```

#### How not to be killed?
```erlang
  process_flag(trap_exit, true).
```

#### Send a message to a process in a different node if you know its *registered_name*
```erlang
  {'name-of-the-process', 'gold_fever@server.ip.goes.here'} ! message.
  % or… if it's a gen_server…
  gen_server:call({'name-of-the-process', 'gold_fever@server.ip.goes.here'}, message).
```
