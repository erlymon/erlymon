#!/bin/bash
# vars
IPT=/sbin/iptables
# Удаляем старые правила и таблицы
echo " * flushing old rules"
$IPT --flush
$IPT --delete-chain

# Устанавливаем политики по умолчанию
echo " * setting default policies"
$IPT -P INPUT DROP
$IPT -P FORWARD DROP
$IPT -P OUTPUT ACCEPT

# Разрешаем хождение трафика по локальному интерфейсу
echo " * allowing loopback devices"
$IPT -A INPUT -i lo -j ACCEPT
$IPT -A OUTPUT -o lo -j ACCEPT
                                                                                                                                                                                   
# All TCP sessions should begin with SYN                                                                                                                                           
$IPT -A INPUT -p tcp ! --syn -m conntrack --ctstate NEW -j DROP                                                                                                                    
# Разрешаем соединения, инициированные уже установленными подключениями                                                                                                            
$IPT -A INPUT -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT

# Открываем порт 22
echo " * allowing ssh on port 22"
$IPT -A INPUT -p tcp --dport 22 -m conntrack --ctstate NEW -j ACCEPT
#Защита от брутфорса ssh
$IPT -A INPUT -p tcp -i eth0 --syn --dport 22 -m recent --name ssh_bad --set
$IPT -A INPUT -p tcp -i eth0 --syn --dport 22 -m recent --name ssh_bad --update --seconds 60 --hitcount 3 -j DROP

# Открываем порт 53
echo " * allowing dns on port 53"
$IPT -A INPUT -p udp -m udp --dport 53 -j ACCEPT

# Открываем порт 123 (ntp)
echo " * allowing ntp on port 123"
$IPT -A INPUT -p udp -m udp --dport 123 -j ACCEPT

# Открываем порт 80
echo " * allowing http on port 80"
$IPT -A INPUT -p tcp --dport 80 -m conntrack --ctstate NEW -j ACCEPT

# Открываем порт 443
echo " * allowing http on port 443"
$IPT -A INPUT -p tcp --dport 443 -m conntrack --ctstate NEW -j ACCEPT


# Wialon IPS: Открываем порт 20332
echo " * allowing http on port 20332"
$IPT -A INPUT -p tcp --dport 20332 -m conntrack --ctstate NEW -j ACCEPT


# Dislocation TBP: Открываем порт 20102
echo " * allowing http on port 20102"
$IPT -A INPUT -p tcp --dport 20102 -m conntrack --ctstate NEW -j ACCEPT

# Globus TR2: Открываем порт 20256
echo " * allowing http on port 20256"
$IPT -A INPUT -p tcp --dport 20256 -m conntrack --ctstate NEW -j ACCEPT

# Syncting: Открываем порт 22000
echo " * allowing http on port 22000"
$IPT -A INPUT -p tcp --dport 22000 -m conntrack --ctstate NEW -j ACCEPT


# Syncting: Открываем порт 21025
echo " * allowing http on port 21025"
$IPT -A INPUT -p udp --dport 21025 -m conntrack --ctstate NEW -j ACCEPT


# Разрешаем пинги
echo " * allowing ping responses"
$IPT -A INPUT -p ICMP --icmp-type 8 -j ACCEPT

# DROP everything else and Log it
$IPT -A INPUT -j LOG
$IPT -A INPUT -j DROP
#
# Сохраняем
#
echo " * saving settings"
/etc/init.d/iptables save
