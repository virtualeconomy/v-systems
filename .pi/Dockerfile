FROM lopsided/archlinux

RUN pacman -Syyu --noconfirm git jre8-openjdk-headless && \
        git clone https://github.com/faddat/v-systems

COPY vsys-all-0.3.2.jar /v-systems
