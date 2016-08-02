# Add the jessie-backports source to apt
echo 'deb http://ftp.debian.org/debian jessie-backports main' | sudo tee -a /etc/apt/sources.list
sudo apt-get update

# Install certbot
sudo apt-get -y install certbot -t jessie-backports

# Generate cert
sudo certbot certonly --webroot -w /var/www/site -d kerkeslager.com -d www.kerkeslager.com

# Restart nginx
sudo service nginx restart
