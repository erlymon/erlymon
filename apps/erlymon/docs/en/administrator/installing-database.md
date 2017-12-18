# Installing the Database

## Installing MongoDB 3.2 on Ubuntu 16.04 LTS


### Step 1 - Add the MongoDB repository
MongoDB is already included in the Ubuntu package repository, but the official MongoDB repository provides the latest version and is the recommended way to install the software. At this step, we will add this official repository to our server.

Ubuntu guarantees the authenticity of software packages by verifying that they are signed with GPG keys, so we first need to import their key for the official MongoDB repository.

```sh
$ sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv EA312927
```
After successfully importing the key, you will see:

```sh
Output
gpg: Total number processed: 1
gpg:               imported: 1  (RSA: 1)
```

Next, we need to add the MongoDB repository to sources.list.d.

Run the following command to create a file for the MongoDB repository.

```sh
$ sudo echo "deb http://repo.mongodb.org/apt/ubuntu trusty/mongodb-org/3.2 multiverse" | sudo tee /etc/apt/sources.list.d/mongodb-org-3.2.list
```

After adding the repository to the list, you need to update the list of packages

```sh
$ sudo apt-get update
```

### Step 2 - Install and start MongoDB

Now we can install the MongoDB package itself.

```sh
$ sudo apt-get install -y --allow-unauthenticated mongodb-org
```

```sh
Note: MongoDB packages we are using do not meet signature strength standards that Ubuntu 16.04 expects and must be installed with additional --allow-unauthenticated switch.
```

This command will install several packages that contain the latest stable version of MongoDB with useful management tools for MongoDB.

In order to run MongoDB as a service on Ubuntu 16.04, we need to create a file describing the service. This file contains instructions for Systemd. Such as starting or stopping a service, when it should automatically start when the system boots, and whether it depends on other software.

We will create a file to manage the service MongoDB. Create a configuration file named mongodb.service in the directory '/etc/systemd/system' with nano or your favorite text editor.

```sh
$ sudo nano /etc/systemd/system/mongodb.service
```

Insert the following contents, and then save and close the file.

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

The Unit section contains a description (for example, a readable description of the MongoDB service), as well as dependencies that must be run before the service starts. In our case, MongoDB depends on the network, so there's network.target.

The Service section describes how the service should be started. The User directive specifies that the server will work under the MongoDB user, and the ExecStart directive specifies the start command for the MongoDB server.

In the last Install section, specify Systemd when the service should start automatically. Multi-user.target is the standard startup sequence for the system, which means that the server will automatically start at boot time.

Then start the created service with the command systemctl.

```sh
$ sudo systemctl start mongodb
```

You can use systemctl to verify that the service is running.

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

The MongoDB server is configured and running, and you can manage the MongoDB service with the systemctl command (for example, sudo systemctl mongodb stop, sudo systemctl mongodb start).

## Installing MongoDB 3.2 on Gentoo


### Step 1 - Install and start MongoDB

Installing the MongoDB package.

```sh
$ sudo emerge -a mongodb
```

This command will install several packages that contain the latest stable version of MongoDB with useful management tools for MongoDB.

In order to start MongoDB as a service, you need to add a script to OpenRC/SystemD

#### OpenRC

Next add mongodb to autorun

```sh
$ sudo rc-update add mongod default
```

Running mongodb

```sh
$ sudo /etc/init.d/mongod start
```

#### SystemD
