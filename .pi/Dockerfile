FROM faddat/sos-full

RUN pacman -Syyu --noconfirm sbt jdk8-openjdk && \
        git clone https://github.com/faddat/v-systems --branch pi && \
        cd v-systems && \
        sbt packageAll
