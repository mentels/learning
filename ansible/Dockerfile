FROM phusion/baseimage:latest

RUN apt-add-repository ppa:ansible/ansible && \
    apt-get update && \
    apt-get -y install iputils-ping \
                       net-tools \
                       software-properties-common \
                       ansible
                    
# Enable sshd
RUN rm -f /etc/service/sshd/down

# Regenerate SSH host keys. baseimage-docker does not contain any, so you
# have to do that yourself. You may also comment out this instruction; the
# init system will auto-generate one during boot.
RUN /etc/my_init.d/00_regen_ssh_host_keys.sh

# Permanenty enable insecure key and add it as an id_rsa so that
# containers created from this image can connect to each other
RUN /usr/sbin/enable_insecure_key
RUN curl -o id_rsa -fSL https://github.com/phusion/baseimage-docker/raw/master/image/services/sshd/keys/insecure_key && \
    chmod 600 id_rsa && \
    mv id_rsa /root/.ssh/


