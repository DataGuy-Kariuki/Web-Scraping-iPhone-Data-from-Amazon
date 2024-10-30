# Load the rsconnect package for deployment
library(rsconnect)

# Set account information (replace with your actual credentials)
rsconnect::setAccountInfo(name = 'kamwanaanalyst',
                          token = 'F2BECBC274AB97308196FFE5102B6C63',
                          secret = 'Thjz9T/h9u3BTvOvxaeNkmEyBwc6N4xX7kzBl9Ah')

# Deploy the app to ShinyApps.io (replace the path with your app's actual directory)
rsconnect::deployApp('C:/Users/reube/Documents/Visualize.IT/Webscrapping using R/Kariuki.Data.Blog-main')


