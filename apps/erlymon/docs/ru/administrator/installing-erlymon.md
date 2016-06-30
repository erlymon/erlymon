# Установка erlymon

## Установка erlymon на ОС Gentoo

Перед тем как установить  erlymon пакет, необходимо добавить overlay в список layman.
 
~~~sh
$ sudo layman -o https://raw.github.com/pese-git/erlymon-overlay/master/repositories.xml -f -a erlymon-overlay
~~~

Далее обновите дерево пакетов layman
~~~sh
$ sudo layman -s ALL
~~~

Теперь вы можете установить erlymon сервис из дерева портежей
~~~sh
$ sudo emerge -a gps-tracking/erlymon
~~~

Для того чтобы запустить erlymon как сервис необходимо добавить скрипт в OpenRC/SystemD

#### OpenRC

Добавляем erlymon в автозапуск

```sh
$ sudo rc-update add erlymon default
```

Запуск erlymon

```sh
$ sudo /etc/init.d/erlymon start
```

#### SystemD


## Установка erlymon из исходного кода

Исходный код хранится на GitHub

Клонируйте репозиторий

~~~sh
$ git clone https://github.com/pese-git/erlymon.git erlymon
~~~

Перейдите в папку erlymon

~~~sh
$ cd erlymon
~~~

### Есть два способа сборки и запуска сервиса

#### Первый способ

Соберите проект с помощью команды tar

~~~sh
$ ./rebar3 tar
~~~

После окончания сборки в папке _build/default/rel/erlymon/  будет находиться архив erlymon-<vsn>.tar.gz

Этот архив вы можете распаковать в любом месте, но мы его распакуем в папку /opt

Создадим каталог в директории /opt
~~~sh
$ sudo mkdir /opt/erlymon
~~~

Распаковываем архив
~~~sh
$ sudo tar -xvf _build/default/rel/erlymon/erlymon-<vsn>.tar.gz -C /opt/erlymon
~~~

Перед запуском erlymon сервиса необходимо создать два конфигурационных файла sys.conf и vm.args (Шаблоны этих файлов есть в распакованной директории).

Переходим в каталог /opt/erlymon
~~~sh
$ sudo cd /opt/erlymon
~~~

Копируем шаблоны конфигурационных файлов sys.conf.orig и vm.args.orig 

~~~sh
$ sudo cp ./releases/1.0.1/sys.conf.orig ./releases/1.0.1/sys.conf
~~~


~~~sh
$ sudo cp ./releases/1.0.1/vm.args.orig ./releases/1.0.1/vm.args
~~~

Запустите сервис в режиме консоль

~~~sh
$ sudo sh bin/erlymon console
~~~

Также можно запустить/остановить сервис в режиме демона

Запуск демона

~~~sh
$ sh bin/erlymon start
~~~

Остановка демона

~~~sh
$ sh bin/erlymon stop
~~~


#### Второй способ

Соберите проект с помощью команды release

~~~sh
$ ./rebar3 release
~~~

Запустите сервис в режиме консоль

~~~sh
$ sh _build/default/rel/erlymon/bin/erlymon console
~~~

Также можно запустить/остановить сервис в режиме демона

Запуск демона

~~~sh
$ sh _build/default/rel/erlymon/bin/erlymon start
~~~

Остановка демона

~~~sh
$ sh _build/default/rel/erlymon/bin/erlymon stop
~~~
