import os
import subprocess


os.system('sudo service postgresql stop')
# os.system('sudo docker-compose build')
# os.system('sudo docker-compose up')

processes = []

processes.append(subprocess.Popen(['sudo', 'docker-compose', 'up'], cwd='./DB'))

for p in processes:
    p.wait()
