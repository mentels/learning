.PHONY: build ansiansible_tester ansible_under_test

image = local/pbi-ansible
ip_cmd = docker inspect -f '{{ .NetworkSettings.IPAddress }}'
aut1 = ansible_under_test1
aut2 = ansible_under_test2


# BUILDING
build:
	docker build . -t $(image)

rmi:
	docker rmi $(image)

# RUNNING
ansible_under_test1:
	docker run --name $@ -dt $(image) /sbin/my_init

ansible_under_test2:
	docker run --name $@ -dt $(image) /sbin/my_init

ansible_tester: ansible_hosts
	docker run --name $@ -dt $(image) /sbin/my_init
	docker cp ansible_hosts $@:/root/hosts

ansible_hosts:
	ip1=`$(ip_cmd) $(aut1)` && \
	ip2=`$(ip_cmd) $(aut2)` && \
	echo "[servers]\n$$ip1\n$$ip2" > $@


# BASH
bash_ansible_under_test1:
	docker exec -it ansible_under_test1 bash -l

bash_ansible_under_test2:
	docker exec -it ansible_under_test2 bash -l

bash_ansible_tester:
	docker exec -it ansible_tester bash -l

# IP

ip_ansible_under_test:
	docker exec -it ansible_under_test ifconfig

ip_ansible_under_test2:
	docker exec -it ansible_under_test2 ifconfig

ip_ansible_tester:
	docker exec -it ansible_tester ifconfig

# SAMPLE COMMANDS

uptime:
	docker exec ansible_tester ansible servers -a "uptime" -i /root/hosts

htop:
	docker exec ansible_tester ansible servers -m apt -a "name=htop" -i /root/hosts

## Seems to be not working
reboot:
	docker exec ansible_tester ansible servers -a "/sbin/reboot" -i /root/hosts
