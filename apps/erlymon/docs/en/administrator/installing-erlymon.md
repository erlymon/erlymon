# Installing the Erlymon service

## Installing Erlymon service on   Gentoo OS

Before installing the erlymon package, you need to add an overlay to the layman.

~~~sh
$ sudo layman -o https://raw.github.com/pese-git/erlymon-overlay/master/repositories.xml -f -a erlymon-overlay
~~~

Next, update the layman package tree
~~~sh
$ sudo layman -s ALL
~~~

Now you can install the erlymon service from the portage tree
~~~sh
$ sudo emerge -a gps-tracking/erlymon
~~~

If you want run erlymon as a daemon, you need to add a script to OpenRC / SystemD

#### OpenRC

Adding erlymon service to autorun

```sh
$ sudo rc-update add erlymon default
```

Running erlymon

```sh
$ sudo /etc/init.d/erlymon start
```

#### SystemD


## Installing erlymon from source code

The source code is stored on GitHub

You must are cloning the source code

~~~sh
$ git clone https://github.com/pese-git/erlymon.git erlymon
~~~

Next go to the erlymon folder

~~~sh
$ cd erlymon
~~~

### There are two ways to build and run the erlymon

#### The first way

Build the project with the tar command

~~~sh
$ ./rebar3 tar
~~~

After the build is finished, the folder "_bulld/default/rel/erlymon/" will contain the archive "erlymon-<vsn>.tar.gz"

You can unpack this archive anywhere, but we will unpack it to the folder "/opt"

Next create a directory in the  directory "/opt"
~~~sh
$ sudo mkdir /opt/erlymon
~~~

Next unpack the archive
~~~sh
$ sudo tar -xvf _build/default/rel/erlymon/erlymon-<vsn>.tar.gz -C /opt/erlymon
~~~

Before running erlymon service it is necessary to create two configuration files sys.conf and vm.args (the templates of these files are in the unpacked directory).

Next go to the directory /opt/erlymon
~~~sh
$ sudo cd /opt/erlymon
~~~

Next copy the templates of the configuration files sys.conf.orig and vm.args.orig

~~~sh
$ sudo cp ./releases/1.0.1/sys.conf.orig ./releases/1.0.1/sys.conf
~~~


~~~sh
$ sudo cp ./releases/1.0.1/vm.args.orig ./releases/1.0.1/vm.args
~~~

Starting the service in console mode

~~~sh
$ sudo sh bin/erlymon console
~~~

Starting/stopping the service in daemon mode

Running the daemon

~~~sh
$ sh bin/erlymon start
~~~

Stopping the daemon

~~~sh
$ sh bin/erlymon stop
~~~


#### The second way

Build the project using the command "release"

~~~sh
$ ./rebar3 release
~~~

Starting the service in console mode

~~~sh
$ sh _build/default/rel/erlymon/bin/erlymon console
~~~

Starting/stopping the service in daemon mode

Running the daemon

~~~sh
$ sh _build/default/rel/erlymon/bin/erlymon start
~~~

Stopping the daemon

~~~sh
$ sh _build/default/rel/erlymon/bin/erlymon stop
~~~
