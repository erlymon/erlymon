# Erlymon GPS Tracking System
=====

Erlymon is an open source GPS tracking system for various GPS tracking devices.

# License
-----

Erlymon software is licensed under the [AGPL Version 3](http://www.gnu.org/licenses/agpl-3.0.html).

# Install
-----

Clone repository

~~~sh
$ git clone https://github.com/pese-git/erlymon.git
~~~

Go to erlymon dir

~~~sh
$ cd erlymon
~~~

Compile project

~~~sh
$ ./rebar3 compile
~~~

Create release

~~~sh
$ ./rebar3 release
~~~

Run application:

~~~sh
$ sh _build/default/rel/erlymon/bin/erlymon console
~~~

# Doc

[Manual Rus](./apps/erlymon/docs/ru/index.md)
