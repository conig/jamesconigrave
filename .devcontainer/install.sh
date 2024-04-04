export DEBIAN_FRONTEND=noninteractive

echo "Updating package lists..."
sudo apt-get update

echo "Installing packages..."
sudo apt-get install -y libxt6 libzmq3-dev xclip

echo "Installation complete."
