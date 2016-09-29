# Add the jessie-backports source to apt
echo 'deb http://ftp.debian.org/debian jessie-backports main' | sudo tee -a /etc/apt/sources.list
sudo apt-get update

# Install certbot
sudo apt-get -y install certbot -t jessie-backports

# Generate cert
sudo certbot certonly --webroot -w /var/www/site -d kerkeslager.com -d www.kerkeslager.com

# Generate stronger Diffie-Hellman parameters
sudo openssl dhparam -out /etc/letsencrypt/live/kerkeslager.com/dhparam.pem 4096

# Restart nginx
sudo service nginx restart
