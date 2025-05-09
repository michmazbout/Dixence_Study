import gpxpy
import gpxpy.gpx
from geopy.distance import geodesic
import pandas as pd
import csv

# Paths to the GPX files
gpx_file_paths = [
    './data/raw/waypoints/Waypoints_04-SEP-24.gpx',
    './data/raw/waypoints/Waypoints_05-SEP-24.gpx']

# Create a new GPX object to hold the merged waypoints
merged_gpx = gpxpy.gpx.GPX()

# Iterate through the files and extract waypoints
for gpx_file_path in gpx_file_paths:
    with open(gpx_file_path, 'r') as gpx_file:
        gpx = gpxpy.parse(gpx_file)
        for waypoint in gpx.waypoints:
            merged_waypoint = gpxpy.gpx.GPXWaypoint(
                latitude=waypoint.latitude,
                longitude=waypoint.longitude,
                elevation=waypoint.elevation,
                name=waypoint.name,
                description=waypoint.description,
                time=waypoint.time
            )
            merged_gpx.waypoints.append(merged_waypoint)

# Grande Dixence Dam coordinates
dam_coords = (46.08056, 7.40389)

# Extract waypoints from GPX
waypoints = []
for waypoint in merged_gpx.waypoints:
	waypoints.append({
		"name": waypoint.name,
		"latitude": waypoint.latitude,
		"longitude": waypoint.longitude
	})

# Calculate distances
distances = []
for waypoint in waypoints:
	point_coords = (waypoint["latitude"], waypoint["longitude"])
	distance = geodesic(dam_coords, point_coords).kilometers
	distances.append({
		"Waypoint": waypoint["name"],
		"Latitude": waypoint["latitude"],
		"Longitude": waypoint["longitude"],
		"Distance_km": distance
	})

# Convert results to a DataFrame
df_distances = pd.DataFrame(distances)

# Remove extra last 2 waypoints as they do not correspond to actual images
df_distances = df_distances.iloc[0:240]
# Fix the first point
df_distances.iloc[0, -1] = df_distances.iloc[1, -1]

# Save to a CSV file
output_path = './data/waypoints_distances.csv'
df_distances.to_csv(output_path, index=False, quoting=csv.QUOTE_ALL, quotechar='"')

print(f"Distances saved to {output_path}")