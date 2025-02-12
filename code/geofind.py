
# https://cran.r-project.org/web/packages/geodist/geodist.pdf
# https://jessecambon.github.io/tidygeocoder/
# https://jessecambon.github.io/tidygeocoder/articles/geocoder_services.html
# https://medium.com/@arpit23sh/discover-nearby-places-with-python-a-geolocation-adventure-770ecc78f13f

from geopy.geocoders import Nominatim
from geopy.geocoders import GoogleV3
from geopy.distance import geodesic
import requests

def get_user_location():
    try:
        response = requests.get('https://ipinfo.io')
        data = response.json()
        return data['loc'].split(',')
    except:
        print("Error: Unable to detect your location.")
        return None, None

def find_nearby_places(lat, lon, place_type, radius):
    geolocator = Nominatim(user_agent="nearby_search_vishal")
    location = geolocator.reverse((lat, lon))
    print(f"\nYour current location: {location}\n")
    
    print((location.latitude, location.longitude))

    #print(location.raw)

    query = f"{place_type} near {location.latitude}, {location.longitude}"
    print(query)
    try:
        places = geolocator.geocode(query, exactly_one=False, limit=None)
        if places:
            for place in places:
                place_coords = (place.latitude, place.longitude)
                place_distance = geodesic((lat, lon), place_coords).kilometers
                if place_distance <= radius:
                    print(f"{place.address} ({place_distance:.2f} km)")
        else:
            print("No nearby places found for the given type.")
    except:
        print("Error: Unable to fetch nearby places.")
        
if __name__ == "__main__":
    #user_lat, user_lon = get_user_location()
    user_lat = 52.50017195775029 
    user_lon = 13.317122214370833
    #user_lat, user_lon = float(input("Enter the latitude and longitude:").split())
    
    #print(user_lat, user_lon)

    if user_lat is not None and user_lon is not None:
        place_type = input("What type of place are you looking for? (e.g., park, mall, ATM, hotel): ")
        search_radius = float(input("Enter the search radius (in kilometers): "))
        find_nearby_places(float(user_lat), float(user_lon), place_type, search_radius)