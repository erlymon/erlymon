# Erlang Release init.d script

If you have packaged your application into a release generated with [Rebar3](https://github.com/erlang/rebar3), you might want to have the following: 

 * The release started on system boot.
 * The VM monitored and restarted it if it crashes.

### Use HEART
HEART is the [Heartbeat Monitoring of an Erlang Runtime System](http://www.erlang.org/doc/man/heart.html). The purpose of the heart port program is to check that the Erlang runtime system it is supervising is still running, so that if the VM crashes or becomes unresponsive, it is restarted.

To do so, you just have to ensure that the `-heart` option is uncommented in your `vm.args` file.

### `init.d` script
Create the file `/etc/init.d/erlymon`:

```bash
$ sudo vim /etc/init.d/erlymon
```

Paste the following:

```bash
#!/usr/bin/env bash
# erlymon daemon
# chkconfig: 345 20 80
# description: erlymon daemon
# processname: erlymon

NAME=erlymon
PROJECT_ROOT_PATH=/usr/local/$NAME
APP_SCRIPT="${PROJECT_ROOT_PATH}/bin/$NAME"

# export
export HOME=/root

case "$1" in
start)
    printf "%-50s" "Starting $NAME..."

    # start
    cd $PROJECT_ROOT_PATH
    $APP_SCRIPT start > /dev/null 2>&1;

    # wait for pid
    for (( i=0; i<10; ++i )); do
        OUT=`$APP_SCRIPT getpid`;
        if [ $? == 0 ]; then PID=$OUT; break; fi
        sleep 1;
    done

    if [ -z "$PID" ]; then
        printf "%s\n" "Failsd"
    else
        printf "%s\n" "Ok"
    fi
;;
status)
    printf "%-50s" "Checking $NAME..."

    # wait for pid
    cd $PROJECT_ROOT_PATH
    $APP_SCRIPT getpid > /dev/null 2>&1;

    if [ $? != 0 ]; then
        printf "%s\n" "Node is not running!"
    else
        printf "%s\n" "Ok"
    fi
;;
stop)
    printf "%-50s" "Stopping $NAME..."

    # cd and stop
    cd $PROJECT_ROOT_PATH
    $APP_SCRIPT stop > /dev/null 2>&1;

    if [ $? != 0 ]; then
        printf "%s\n" "Node is not running!"
    else
        printf "%s\n" "Ok"
    fi
;;

restart)
    $0 stop
    $0 start
;;

*)
    echo "Usage: $0 {status|start|stop|restart}"
    exit 1
esac
```

> Side note: you can see that the script waits to exit the start function until the PID is retrieved from the VM.
> This is not strictly necessary, although in this way you can consider dumping it into PID files or perform other types of monitoring actions besides using HEART (monit, god, ...)


After saving, ensure that this file is executable:

```bash
$ sudo update-rc.d erlymon defaults
```

You now have a new service:

```
$ sudo service erlymon start
Starting erlymon...                                Ok
```

### Run at boot
Ensure that the service is started at boot time:

```bash
$ sudo update-rc.d erlymon defaults
```