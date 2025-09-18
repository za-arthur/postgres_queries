# Capturing stacks

With dtrace:

```bash
dtrace -x ustackframes=100 -n 'profile-97 /execname == "postgres" && arg1/ { @[ustack()] = count(); } tick-60s { exit(0); }' -o out.user_stacks
```

With perf:

```bash
perf record -F 99 -g --all-user -a --call-graph dwarf -- sh -c 'pgrep postgres > /dev/null; sleep 60'
```

# Memory usage

Check memory usage:

```bash
ps -e -o pid,ppid,cmd,%mem,%cpu --sort=-%mem | grep postgres
```

Check memory usage of specific process:

```bash
ps -o rss= <pid> | awk '{printf "%.0f\n", $1 / 1024}'
```