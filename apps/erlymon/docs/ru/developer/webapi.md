# WebApi

Доступ к api можно получить по ссылке http://localhost:8082/api

На любой запрос, если у вас есть права и сервер функционирует в штатном режиме, сервер отвечает json объектом либо json массивом.
Иначе сервер возвращает код ошибки с сообщением.

Для доступа к web api вместо curl используем httpie

## Сессия

Для того, чтобы иметь доступ к функциям создания/удаления/редактирования пользоватлей/устройств/прав доступа необходимо авторизоватся в системе.

Для авторизации необходимо открыть сессию, либо убедиться, что ваша сессия ещё жива (т.е. получить параметры сессии)

Как только вы закончили работу с сервером вам необходимо закрыть сессию.

### Получить сессию
```sh
$ http localhost:8082/api/session 'Cookie:session=7cafa2ff-8e17-416c-955e-e16faa2fb2ac'

HTTP/1.1 200 OK
Connection: keep-alive
Content-Length: 106
Content-Type: application/json; charset=UTF-8
Date: Fri, 01 Jul 2016 10:02:28 GMT
Keep-Alive: timeout=20
Server: nginx/1.8.1

{                                                                                                                                                                                 
    "admin": true,                                                                                                                                                                
    "distanceUnit": "km",                                                                                                                                                         
    "email": "admin",                                                                                                                                                             
    "id": 1,                                                                                                                                                                      
    "language": "en",                                                                                                                                                             
    "latitude": 53.0,                                                                                                                                                             
    "longitude": 28.0,                                                                                                                                                            
    "map": "osm", 
    "name": "admin", 
    "password": null, 
    "readonly": false, 
    "speedUnit": "kmh", 
    "zoom": 10
}
```

### Открыть сессию
```sh
$ http -f POST localhost:8082/api/session email=admin password=admin

HTTP/1.1 200 OK
content-length: 174
content-type: application/json; charset=UTF-8
date: Fri, 01 Jul 2016 12:42:22 GMT
server: Cowboy
set-cookie: session=76e93a36-02d6-4f34-a518-a982086bf52c; Version=1; Path=/

{
    "admin": true, 
    "distanceUnit": "km", 
    "email": "admin", 
    "id": 1, 
    "language": "en", 
    "latitude": 53.0, 
    "longitude": 28.0, 
    "map": "osm", 
    "name": "admin", 
    "password": null, 
    "readonly": false, 
    "speedUnit": "kmh", 
    "zoom": 10
}

```

### Закрыть сессию
```sh
$ http DELETE localhost:8082/api/session 'Cookie:session=7cafa2ff-8e17-416c-955e-e16faa2fb2ac'

Connection: keep-alive
Content-Length: 0
Date: Fri, 01 Jul 2016 10:06:12 GMT
Keep-Alive: timeout=20
Server: nginx/1.8.1
set-cookie: session=deleted; Version=1; Path=/



```

## Сервер

Доступ к параметрам сервера имеют все пользователи.
Доступ к редактированию параметров сервера имеет только администратор.

### Получить параметры сервера

```sh
$ http localhost:8082/api/server

HTTP/1.1 200 OK
content-length: 171
content-type: application/json; charset=UTF-8
date: Fri, 01 Jul 2016 12:40:42 GMT
server: Cowboy

{
    "bingKey": "", 
    "distanceUnit": "", 
    "id": 1, 
    "language": null, 
    "latitude": 53.0, 
    "longitude": 27.0, 
    "map": "12311111", 
    "mapUrl": "", 
    "readonly": false, 
    "registration": true, 
    "speedUnit": "", 
    "zoom": 10
}

```

### Обновить параметры сервера

```sh
$ http --json PUT localhost:8082/api/server 'Cookie:session=cc3aee99-f4dd-459f-9b66-e9d304a5aa2f; Version=1; Path=/' id:=1465545256 bingKey= distanceUnit= language= latitude:=0 longitude:=0 map=osm mapUrl= readonly:=false readonly:=true speedUnit= zoom:=0

HTTP/1.1 200 OK
content-length: 153
content-type: application/json; charset=UTF-8
date: Fri, 01 Jul 2016 13:58:47 GMT
server: Cowboy

{
    "bingKey": "",
    "distanceUnit": "",
    "id": 1465545256,
    "language": "",
    "latitude": 0,
    "longitude": 0,
    "map": "osm",
    "mapUrl": "",
    "readonly": true,
    "speedUnit": "",
    "zoom": 0
}
```

## Пользователи

Доступ к пользователям имеет только администратор. Т.е. получить/добавить/редактировать/удалить пользователей может только администратор.
Доступ к регистрации пользователя есть у всех.
Доступ к обновлению пользователя есть у всех, если это авторизованный пользователь.

### Получить список пользователей

```sh
$ http GET localhost:8082/api/users 'Cookie:session=76e93a36-02d6-4f34-a518-a982086bf52c; Version=1; Path=/'

HTTP/1.1 200 OK
content-length: 176
content-type: application/json; charset=UTF-8
date: Fri, 01 Jul 2016 12:43:42 GMT
server: Cowboy

[
    {
        "admin": true, 
        "distanceUnit": "km", 
        "email": "admin", 
        "id": 1, 
        "language": "en", 
        "latitude": 53.0, 
        "longitude": 28.0, 
        "map": "osm", 
        "name": "admin", 
        "password": null, 
        "readonly": false, 
        "speedUnit": "kmh", 
        "zoom": 10
    }
]

```

### Создать/Зарегистрировать пользователя

```
$ http --json POST localhost:8082/api/users 'Cookie:session=cc3aee99-f4dd-459f-9b66-e9d304a5aa2f; Version=1; Path=/' name=manager email=manager@manager.com password=manager

HTTP/1.1 200 OK
content-length: 176
content-type: application/json; charset=UTF-8
date: Fri, 01 Jul 2016 12:43:42 GMT
server: Cowboy

{
    "admin": false, 
    "distanceUnit": null, 
    "email": "manager@manager.com", 
    "id": 37, 
    "language": null, 
    "latitude": 0.0, 
    "longitude": 0.0, 
    "map": null, 
    "name": "manager", 
    "password": "manager", 
    "readonly": false, 
    "speedUnit": null, 
    "zoom": 0
}

```

### Обновить пользователя

```
http --json PUT localhost:8082/api/users/1467380757 'Cookie:session=cc3aee99-f4dd-459f-9b66-e9d304a5aa2f; Version=1; Path=/' id:=37 name=manager1 email=manager@manager.com password=manager admin:=false map=null language=null distanceUnit=null speedUnit=null latitude:=0 longitude:=0 zoom:=0

HTTP/1.1 200 OK
content-length: 85
content-type: application/json; charset=UTF-8
date: Fri, 01 Jul 2016 13:50:48 GMT
server: Cowboy

{
    "admin": false, 
    "distanceUnit": "null", 
    "email": "manager@manager.com", 
    "id": 37, 
    "language": "null", 
    "latitude": 0.0, 
    "longitude": 0.0, 
    "map": "null", 
    "name": "manager1", 
    "password": "manager", 
    "readonly": false, 
    "speedUnit": "null", 
    "zoom": 0
}

```

### Удалить пользователя

```sh
$ http --json DELETE localhost:8082/api/users/37 'Cookie:JSESSIONID=1e3lt2gnqxzj1sk5gigg29s94;Path=/api' id:=37

HTTP/1.1 200 OK
content-length: 85
content-type: application/json; charset=UTF-8
date: Fri, 01 Jul 2016 13:51:27 GMT
server: Cowboy
```

## Устройство

Доступ к устройствам есть у всех. Т.е. получить/добавить/редактировать/удалить устройства может любой пользователь.

### Получить список устройств

```sh
$ http GET localhost:8082/api/devices 'Cookie:session=76e93a36-02d6-4f34-a518-a982086bf52c; Version=1; Path=/'

HTTP/1.1 200 OK
content-length: 134
content-type: application/json; charset=UTF-8
date: Fri, 01 Jul 2016 12:46:22 GMT
server: Cowboy

[
    {
        "id": 1, 
        "lastUpdate": "2016-07-28T09:52:47.593+0000", 
        "name": "wialon", 
        "positionId": 10, 
        "status": "offline", 
        "uniqueId": "001"
    }
]

```

### Создать устройство
```sh
$ http --json POST localhost:8082/api/devices 'Cookie:session=c914b9fd-ac19-48f1-883d-3fc3eb286a70; Version=1; Path=/' id=-1 name=tractor uniqueId=tractor

HTTP/1.1 200 OK
content-length: 112
content-type: application/json; charset=UTF-8
date: Fri, 01 Jul 2016 13:25:34 GMT
server: Cowboy

{
    "id": 39, 
    "lastUpdate": null, 
    "name": "tractor", 
    "positionId": 0, 
    "status": null, 
    "uniqueId": "tractor"
}


```

### Обновить устройство

```sh
$ http --json PUT localhost:8082/api/devices/39 'Cookie:session=91b7a488-cc80-4ceb-a12e-fb9f2bb48a20' id:=39 name=tractor uniqueId=tractor

HTTP/1.1 200 OK
content-length: 128
content-type: application/json; charset=UTF-8
date: Fri, 01 Jul 2016 13:32:25 GMT
server: Cowboy

{
    "id": 39, 
    "lastUpdate": null, 
    "name": "tractor", 
    "positionId": 0, 
    "status": null, 
    "uniqueId": "tractor"
}

```

### Удалить устройство

```sh
$ http --json DELETE localhost:8082/api/devices/39 'Cookie:session=91b7a488-cc80-4ceb-a12e-fb9f2bb48a20' id:=39

HTTP/1.1 204 No Content
Access-Control-Allow-Credentials: true
Access-Control-Allow-Headers: origin, content-type, accept, authorization
Access-Control-Allow-Methods: GET, POST, PUT, DELETE, OPTIONS
Access-Control-Allow-Origin: *
Date: Wed, 10 Aug 2016 13:55:24 GMT
server: Cowboy

```

## Права доступа

Права доступа может редактировать только администратор.

### Добавить доступ к устройству

```sh
http --json POST localhost:8082/api/permissions 'Cookie:session=cc3aee99-f4dd-459f-9b66-e9d304a5aa2f; Version=1; Path=/' userId:=9 deviceId:=9

HTTP/1.1 200 OK
Access-Control-Allow-Credentials: true
Access-Control-Allow-Headers: origin, content-type, accept, authorization
Access-Control-Allow-Methods: GET, POST, PUT, DELETE, OPTIONS
Access-Control-Allow-Origin: *
Content-Length: 25
Content-Type: application/json
Date: Wed, 10 Aug 2016 13:58:10 GMT
server: Cowboy

{
    "deviceId": 1, 
    "userId": 9
}

```

### Удалить доступ к устройству

```sh
$ http --json DELETE localhost:8082/api/permissions 'Cookie:session=cc3aee99-f4dd-459f-9b66-e9d304a5aa2f; Version=1; Path=/' userId:=9 deviceId:=9
HTTP/1.1 204 No Content
Access-Control-Allow-Credentials: true
Access-Control-Allow-Headers: origin, content-type, accept, authorization
Access-Control-Allow-Methods: GET, POST, PUT, DELETE, OPTIONS
Access-Control-Allow-Origin: *
Date: Wed, 10 Aug 2016 13:59:10 GMT
server: Cowboy


```

## Сообщения

### Выгрузка сообщений

```sh
$ http GET localhost:8082/api/positions 'Cookie:session=feabd330-f679-421b-bd95-ed5ee5520af8; Version=1; Path=/' deviceId==1 from==2016-07-01T14:03:56.000Z to==2016-08-01T14:33:56.000Z

HTTP/1.1 200 OK
content-length: 2
content-type: application/json; charset=UTF-8
date: Fri, 01 Jul 2016 14:37:50 GMT
server: Cowboy

[
    {
        "address": null, 
        "altitude": 0.0, 
        "attributes": {
            "ip": "127.0.0.1"
        }, 
        "course": 0.0, 
        "deviceId": 1, 
        "deviceTime": "2016-07-22T07:33:06.000+0000", 
        "fixTime": "2016-07-22T07:33:06.000+0000", 
        "id": 1, 
        "latitude": 53.90553216666667, 
        "longitude": 27.502491833333334, 
        "outdated": false, 
        "protocol": "wialon", 
        "serverTime": "2016-07-22T07:33:25.273+0000", 
        "speed": 0.0, 
        "type": null, 
        "valid": false
    }
]
```

## Команды

### Выполнить команду

```sh
$ http --json POST localhost:8082/api/commands 'Cookie:session=cc3aee99-f4dd-459f-9b66-e9d304a5aa2f; Version=1; Path=/' id:=-1 deviceId:=1 type=engineResume
HTTP/1.1 403 Forbidden
content-length: 20
date: Fri, 01 Jul 2016 14:10:38 GMT
server: Cowboy

{
    "details": "exception", 
    "message": null
}

```

## Соединение по WS

```sh
$ http --json GET localhost:8082/api/socket 'Cookie:session=cc3aee99-f4dd-459f-9b66-e9d304a5aa2f; Version=1; Path=/'

Accept-Encoding:gzip, deflate, sdch
Accept-Language:ru-RU,ru;q=0.8,en-US;q=0.6,en;q=0.4
Cache-Control:no-cache
Connection:Upgrade
Cookie:JSESSIONID=5fwb90b9nlzo19bm3w194etgv
Host:localhost:8082
Origin:http://localhost:8082
Pragma:no-cache
Sec-WebSocket-Extensions:permessage-deflate; client_max_window_bits
Sec-WebSocket-Key:GO+VzBUmBviMg4vxxZHzXA==
Sec-WebSocket-Version:13
Upgrade:websocket
User-Agent:Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.106 Safari/537.36

$ ws://localhost:8082/api/socket

Request Method:GET
Status Code:101 Switching Protocols

{"positions":[{"fixTime":"2016-08-11T09:39:38.000+03:00","latitude":53.905508,"longitude":27.502530666666665,"outdated":false,"valid":false,"altitude":0.0,"speed":0.0,"course":0.0,"deviceTime":"2016-08-11T09:39:38.000+03:00","id":11,"protocol":"wialon","attributes":{"ip":"127.0.0.1"},"deviceId":1}]}

{"devices":[{"status":"online","uniqueId":"001","lastUpdate":"2016-08-11T09:45:00.047+03:00","positionId":10,"name":"wialon","id":1}]}

{"positions":[{"fixTime":"2016-08-11T09:44:50.000+03:00","latitude":53.90554866666667,"longitude":27.5024825,"outdated":false,"valid":false,"altitude":0.0,"speed":0.0,"course":0.0,"deviceTime":"2016-08-11T09:44:50.000+03:00","id":13,"protocol":"wialon","attributes":{"ip":"127.0.0.1"},"deviceId":1}]}

{"devices":[{"status":"offline","uniqueId":"001","lastUpdate":"2016-08-11T09:45:00.047+03:00","positionId":13,"name":"wialon","id":1}]}


```