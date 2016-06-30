# Установка базы данных

## Установка MongoDB 3.2 на Ubuntu 16.04 LTS


### Шаг 1 - Добавление MongoDB репозитория
MongoDB уже включена в  репозиторий пакетов Ubuntu, но официальный репозиторий MongoDB предоставляет самую свежую версию и является рекомендуемым способом установки программного обеспечения. На этом шаге мы добавим этот официальный репозиторий на наш сервер.

Ubuntu гарантирует подлинность пакетов программного обеспечения, проверив, что они подписаны с ключами GPG, поэтому мы сначала должны импортировать их ключ для официального репозитория MongoDB.
```sh
$ sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv EA312927
```
После успешного импорта ключа, вы увидите:

```sh
Output
gpg: Total number processed: 1
gpg:               imported: 1  (RSA: 1)
```

Далее, мы должны добавить MongoDB репозиторий в sources.list.d.

Выполните следующую команду, чтобы создать файл для репозитория MongoDB.

```sh
$ sudo echo "deb http://repo.mongodb.org/apt/ubuntu trusty/mongodb-org/3.2 multiverse" | sudo tee /etc/apt/sources.list.d/mongodb-org-3.2.list
```

После добавления репозитория в список необходимо обновить список пакетов

```sh
$ sudo apt-get update
```

### Шаг 2 - Установка и запуск MongoDB
Теперь мы можем установить сам пакет MongoDB.

```sh
$ sudo apt-get install -y --allow-unauthenticated mongodb-org
```

```sh
Note: MongoDB packages we are using do not meet signature strength standards that Ubuntu 16.04 expects and must be installed with additional --allow-unauthenticated switch. 
```

Эта команда  установит несколько пакетов, которые содержат последнюю стабильную версию MongoDB с полезными инструментами управления для MongoDB.

Для того чтобы запустить MongoDB как сервис на Ubuntu 16.04, мы должны создать файл, описывающий службу. Этот файл содержит инструкции для Systemd. Такие как запуск или остановка сервиса, когда она должна автоматически запускаться при загрузке системы, и зависит ли она от другого программного обеспечения.

Мы создадим файл для управления службой MongoDB. Создайте файл конфигурации с именем mongodb.service в каталоге /etc/systemd/system с помощью nano или ваш любимый текстовый редактор.

```sh
$ sudo nano /etc/systemd/system/mongodb.service
```

Вставте следующее содержимое, а затем сохраните и закройте файл.

```sh
/etc/systemd/system/mongodb.service
[Unit]
Description=High-performance, schema-free document-oriented database
After=network.target

[Service]
User=mongodb
ExecStart=/usr/bin/mongod --quiet --config /etc/mongod.conf

[Install]
WantedBy=multi-user.target
This file has a simple structure:
```

Секция Unit содержит описание (например, удобочитаемый описания сервиса MongoDB), а также зависимости, которые должны быть запущены до старта сервиса. В нашем случае, MongoDB зависит от работы сети, поэтому есть network.target.

Секция Service, описывает, как должна быть запущена служба. Директива User указывает, на то что сервер будет работать под пользователем MongoDB, а директива ExecStart определяет команду запуска для сервера MongoDB.

В последней секции Install, указываем Systemd, когда служба должна запускаться автоматически. Multi-user.target является стандартная последовательность запуска системы, что означает, что сервер будет автоматически запускаться при загрузке.

Затем запустите созданную службу с помощью команды systemctl.

```sh
$ sudo systemctl start mongodb
```

Вы можете использовать systemctl, для того чтобы проверить, то что служба запущена.

```sh
$ sudo systemctl status mongodb
```

```sh
Output
● mongodb.service - High-performance, schema-free document-oriented database
   Loaded: loaded (/etc/systemd/system/mongodb.service; enabled; vendor preset: enabled)
   Active: active (running) since Mon 2016-04-25 14:57:20 EDT; 1min 30s ago
 Main PID: 4093 (mongod)
    Tasks: 16 (limit: 512)
   Memory: 47.1M
      CPU: 1.224s
   CGroup: /system.slice/mongodb.service
           └─4093 /usr/bin/mongod --quiet --config /etc/mongod.conf
The last step is to enable automatically starting MongoDB when the system starts.
```

```sh
$ sudo systemctl enable mongodb
```

Сервер MongoDB настроен и работает, и вы можете управлять службой MongoDB с помощью команды systemctl (например sudo systemctl mongodb stop, sudo systemctl mongodb start).

## Установка MongoDB 3.2 на Gentoo


### Шаг 1 - Установка и запуск MongoDB

Установка пакета MongoDB.

```sh
$ sudo emerge -a mongodb
```

Эта команда  установит несколько пакетов, которые содержат последнюю стабильную версию MongoDB с полезными инструментами управления для MongoDB.

Для того чтобы запустить MongoDB как сервис необходимо добавить скрипт в OpenRC/SystemD

#### OpenRC

Добавляем mongodb в автозапуск

```sh
$ sudo rc-update add mongod default
```

Запуск mongodb

```sh
$ sudo /etc/init.d/mongod start
```

#### SystemD