# docker run -d -p 4444:4444 -p 5900:5900 -p 6900:6900 --shm-size=2g selenium/standalone-chrome-debug:3.141.59-20210607
# https://www.youtube.com/watch?v=SCHs3XMf1yY Thank you my guy.

# NoVNC being set up through the init dockerfile:
# docker build -t novnc .

# new run commands:

# docker network create selenium-net
# docker run -d --name selenium --network selenium-net -p 4444:4444  -p 5900:5900  --shm-size=2g  selenium/standalone-chrome-debug:3.141.59-20210607
# docker run -d --name novnc_with_net --network selenium-net -p 6900:6900  novnc

# Working containers are selenium and novcn_with_net

# new git trigger test