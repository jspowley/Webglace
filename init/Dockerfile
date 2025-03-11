FROM ubuntu:20.04

RUN apt-get update && apt-get install -y \
    git \
    python3 \
    python3-pip && \
    rm -rf /var/lib/apt/lists/*

RUN git clone https://github.com/novnc/noVNC.git /opt/novnc && \
    git clone https://github.com/novnc/websockify.git /opt/websockify

EXPOSE 6900

CMD ["/opt/websockify/run", "--web", "/opt/novnc", "6900", "selenium_for_webglace:5900"]
