# shad-migration-analysis

The analysis of the spawning migration phase of twaite shad tagged and tracked in the Schelde Estuary.





\## Project structure



\### Data



<mark>Data last updated on 06/01/2026</mark>



\* `/raw:`

&#x09;+ `raw\_detection\_data.csv`: dataset containing the raw detection data obtained from the ETN database

&#x09;+ `shad\_meta\_data.csv`: dataset containing the meta-data on the tagged twaite shads obtained from the ETN database

&#x09;+ `deployments.csv`: dataset containing the station names and positions of the receivers from ETN

&#x09;+ `tag\_specifications.csv`: dataset containing the specifications of the used transmitters from ETN



\* `/interim:`

&#x09;+ `receivernetwork\_shad.csv`: file containing the stations where eels have been detected

&#x09;+ `residency.csv`: detection dataset with arrival and departure time according to a specific time threshold, calculated via the `smooth\_tracks.R` code

&#x09;+ `speed.csv`: file with movement speed and distances, calculated via `calculate\_speed.R` code

&#x09;+ `data\_with\_flag\_spawning\_migration.csv`: file with spawning event classified as upstream migration, spawning and downstream migration

&#x09;+ `data\_with\_behaviour\_types.csv`: file with a column 'behaviour' containing all classified behaviours, i.e., tagging effect, foraging, upstream spawning migration, spawning and downstream spawning migration



\* `/external:`

&#x09;+ `release\_locations\_stations.csv`: file with the release locations and the release station names.

&#x09;+ `distancematrix\_shad.csv`: distance matrix of the detection station network (matrices are created at https://github.com/inbo/fish-tracking).

&#x09;+ `station\_locations.csv`: geographical locations of the different detection stations

&#x09;+ `receivernetwork\_shad.csv`: coordinates of the receiver stations used in the shad analysis, obtained from https://github.com/inbo/fish-tracking.

&#x09;+ `station\_order.csv`: file containing the stations upstream the release location. This file is needed in `calculate\_speed.R`

&#x09;+ `shapefiles/`: folder with shapefiles

&#x20;   		+ shad: shape file of the river

&#x20;  		+ shad\_marine: shapefile of estuary and northsea

&#x20;   		+ study area: combines two above

&#x09;+ `rasterfiles/:`

&#x20;   		+ two raterfiles of the study area (resolution 25m and 100m), needed to calculate the distance of the measurement locations to reference point

&#x09;+ `deployments\_habitats.csv`: file with deployments linked to locations, habitat types, windmill areas, aggregate extraction areas and shipping wrecks

&#x09;+ `locations\_environmental\_stations\_westerschelde.csv`: coordinates of the environmental stations in WGS84 format.

&#x09;+ `mean\_temp\_march.csv`: file with mean temperature for the month March in every year. This serves as a proxy for cold vs warm springs.

&#x09;+ `environmental\_variables/`:

&#x09;	+ `westerschelde/`: csv datasets of environmental data in the Westerschelde obtained from the Rijkswaterstaat waterinfo server. For each environmental variable (temperature \[°C], conductivity \[mS/m], dissolved oxygen \[mg/l], waterlevel \[NAP]) a file, containg the values for each location in the Westerschelde

&#x09;	+  `benedenschelde/:`

&#x20;       		+ `coords\_bs.csv`

&#x20;       		+ `lunar\_cycle.csv`

&#x20;       		+ `photoperiod.csv`

&#x20;       		+ `zes...\_tij.csv`

&#x20;       		+ `zes...\_cond.csv`: µS/cm

&#x20;       		+ `zes...\_DO.csv`: mg/l

&#x20;       		+ `zes...\_T.csv`: °C

&#x20;       		+ `zes...\_TURB.csv`: NTU

&#x20;   		+ `metadata.csv:` for each measurement point: station name, location, distance from reference location (=rel\_station4), type of measurement (tide, cond, turb, temp, do), whether the location is downstream from the reference location (is\_downstream).









\### Scripts



\* `/src:`



1\. `download\_data.R`: Download twaite shad acoustic telemetry data from ETN database via RStudio LifeWatch server

&#x09;\* obtain detection dataset `raw\_detection\_data.csv`

&#x09;\* obtain meta-data on tagged shads `shad\_meta\_data.csv`

&#x09;\* obtain meta-data on deployments `deployments.csv` (station names and positions)

2\. `attach\_release.R`: Add shad release positions and date-time to detection dataset

3\. `merge\_shad\_characteristics.R`: Add shad meta data to the detection dataset

4\. `remove\_false.R`: Remove false detections from dataset

5\. `extract\_network.R`: Extract receiver networks based on detection data

&#x09;\* This serves as input to calculate the distance matrices at https://github.com/inbo/fish-tracking

6\. `smooth\_tracks.R`: Smooths duplicates and calculates residencies per shad per station. Therefore, it calls the following two functions:

&#x09;+ 6a. `get\_nearest\_stations.R`: general function to extract the smoothed track for one shad (via its `transmitter ID`)

&#x09;+ 6b. `get\_timeline.R`: function to get the stations which are near a given station (where near means that the distance is smaller than a certain given limit, e.g. detection range).

&#x09;	- --> Generate residency datasets per project and store them in `/interim/residencies`

7\. `calculate\_speed.R`: Calculate movement speeds between consecutive detection stations. Also calculates swim distance, swim time, cumulative swim distance and station distance from source station.

&#x09;+ 7a. `calculate\_speed\_function.R`: function to calculate speed between consecutive displacements; based on a function in Hugo Flavio's `actel` package

&#x09;+ 7b. `calculate\_sourcedistance\_function.R`: function to calculate the station distance from a 'source' station; based on a function in Hugo Flavio's `actel` package

8\. `merge\_shad\_characteristics2.R`: Add shad meta data to the speed dataset

9\. `create\_distance\_plot.R`: Create plots with travelled distance per shad and store as .pdf in `figures\\`.

10\. `explore\_data.R`: Plot number of shads per station, number of stations per shad and tracking time

11\. `add\_location.R`: Add detection location to station names

12\. `flag\_tagging\_effect.R`: Identify tagging effect: the moment the shads are in the BPNS upon tagging and do not return to the Zeeschelde anymore during the first 40 days of tracking

&#x09;+ 12a. `detect\_tagging\_effect.R`: Function to detect tagging effect

13\. `flag\_spawning\_migration.R`: Classify movement behaviours into upstream migration, spawning and downstream migration

&#x09;+ 13a. `detect\_spawning\_migration\_functions\_speed\_method.R`: Functions to detect spawning migration based on speed and distance thresholds

&#x09;+ 13b. `detect\_spawning\_migration\_functions\_first\_derivative\_smoother\_method.R`: Functions to detect spawning migration based on loess and GAM smoothers

14\. `polish\_behaviour.R`: Because the method to flag spawning migration and migration is not 100% (likely impossible given the individual variability), this script manually classifies some parts of the tracks (e.g. end of upstream migration which should be spawning)

15\. `combine\_behaviour.R`: Combine different behaviours (i.e. tagging effect, spawning, upstream spawning migration, downstream spawning migration \& foraging) into single column 'behaviour'.

16\. `plot\_behaviour.R`: Plot the distance track with different colours per behaviour. The plot is stored in `figures\\`.

17\. `clean\_spawning.R`: Clean spawning behaviour data by selecting data from the estuary and remove seaward movement events in a spawning event.

18\. `analyse\_migratory\_fish\_size.R`: Analyse the difference in size between fish that have been selected for the spawning migration analysis.

19\. `analyse\_duration.R`: Analyse the duration of the upstream migration, spawning itself and downstream migration.

20\. `analyse\_period.R`: Analyse the period of the upstream migration, spawning itself and downstream migration.

21\. `analyse\_speed.R`: Analyse the migration speed during upstream and downstream migration.

22\. `analyse\_area.R`: Analyse the size of the spawning area.

23\. `process\_env\_ws.R`: Process environmental data of the Westerschelde obtained via Rijkswaterstaat Waterinfo platform

24\. `analyse\_environmental\_variables.R`: Determine the environmental conditions during upstream migration, spawning and downstream migration, and test for differences between years.

25\. `visualise\_tide.R`: Link water level data to upstream and downstream migration data to identify whether shads apply selective tidal stream transport





\* `/env\_variables/:`

&#x20;   + `config.R`: Store useful variables and configuration.

&#x20;   + `data\_download.R`:

&#x20;       - download environmental variables for benedenscheldt via wateRinfo package (tide, dissolved oxygen, turbidity, conductivity, temperature)

&#x20;       - download photoperiod via suncalc package

&#x20;       - saved at `/external/environmental\_variables/benedenschelde`

&#x20;   + `data\_process.R`:

&#x20;       - source `process\_env\_ws.R`: this loads and processes the environmental data of the westerscheldt (here also the waterlevel data is processed to the times of high water ("HW") and ("LW")). Note: conditivity values of WS were set from mS/m to µS/cm

&#x20;       - create metadata file: save at `/external/environmental\_variables/metadata.csv`

&#x20;       - process detection data: add whether the receiver is up/downstream of the reference point (column is\_down\_tov\_ref). Add whether the fish is migrating up/downstream (column downstream). Saved at `/interim/data\_with\_downstream\_flag.csv`

&#x20;   + `link\_env\_variable.R`:

&#x20;       - source `link\_env\_variables\_functions.R` for all functions

&#x09;- load all required data

&#x20;       - for all variables:

&#x20;           - for each detection:

&#x20;               - function find\_closest\_env\_station: choose the right measurement point

&#x20;               - for this location:

&#x20;                   1) function get\_temp\_series\_for\_station: Get the variable (either from ws or bs) and put them in a standard format.

&#x20;                   2) function find\_closest\_temp\_at\_arrival: Find the closest measurement point in time to the arrival time of the dectection

&#x20;       - for the tide additional calculations are required to visualise and check statistical significance of STST (function calculate\_tide\_characteristics)

&#x09;		1) loads the high water/low water ("HW"/"LW") data

&#x09;		2) calculate for each location the temporal proportions of ebb and flood (required to statistically check STST)

&#x09;		3) calculate for each detection the time after HW (required for visualisation)

&#x09;- save data as `data\_with\_env\_variables.csv` in `/interim/` folder





