brew tap caskroom/cask
brew cask install java --force
java -version
# java >= 1.8 is required.

# Download DAVID SSL certificate
echo -n | openssl s_client -connect david.ncifcrf.gov:443 | sed -ne '/-BEGIN CERTIFICATE-/,/-END CERTIFICATE-/p' > ncifcrf.cert

# Check if it was properly downloaded:
openssl x509 -in ncifcrf.cert -text

# Find the cacerts file:
find /Library/Java/JavaVirtualMachines -name "cacerts"
CACERTS="/Library/Java/JavaVirtualMachines/jdk1.8.0_102.jdk/Contents/Home/jre/lib/security/cacerts"

# Update cacerts in JRE:
sudo keytool -delete -alias david -keystore $CACERTS -storepass changeit -noprompt
sudo cp $CACERTS .
sudo keytool -import -trustcacerts -keystore cacerts -storepass changeit -noprompt -alias david -file ncifcrf.cert
sudo cp cacerts $CACERTS

# Update Java configuration in R:
sudo R CMD javareconf
