# AFL Visual Analysis
A Shiny app to create visual analysis for AFL matches.

## Data Collection tab
The data collection tab contains the sections required to collect the information used in analsyis.  The tab consists of 2 main sections. The top section is the Game Information where basic information about the game that is being analysed is entered.  The second section consists of scaled ovals and buttons that allow for the tagging of various events.

### Game Information section

#### Venue
The Select Venue dropdown contains the majority of AFL venues as well as a Custom selection that provides the user with the ability to input size dimensions for any other venues that might be used for matches.

#### Venue Dimensions
The Venue Length and Venue Width are numeric inputs that are preset with the size dimensions for common AFL venues.  These can be manually adjusted as required and set up the scaled ovals used in both the Data Collection and Visual Output tabs.

#### Teams
The Home Team and Away team dropdowns contain the 18 AFL teams.

#### Team Kicking to the Right
The Team Kicking to the Right in 1st Quarter dropdown is limited to the Home and Away teams that have been selected.  This combined with the Quarter dropdown normalise the collected data to ensure all outputs use the same direction.

#### Quarter
The Quarter dropdown tags each event with the appropriate quarter and helps normalise the direction of the collected data.

#### Ovals
An oval is provided for both the Home and Away teams.  The ovals are to scale for the selected venue. Both collect the x, y location of mouse button clicks.  It's recommended to think of the direction that the play is going as the same direction of the TV broadcast.  That is, if a team is kicking to the right on the TV broadcast, they should be kicking to the right on these plots as well.

#### Buttons
There are 6 buttons available to tag various events.  **At the end of each chain of possessions the End Chain button needs to be clicked.**  Multiple buttons may be clicked with each event, for instance the Retained button and Goal button may be clicked if Inside 50 kicks are being tracked.  Each button is also "hotkeyed" to enable the user to use the keyboard to click the button rather than using the mouse.

##### Hotkeys
Home hotkeys:
Retained - 1
Stoppage - 2
TO - 3
Goal - 4
Behind - 5
End Chain - 6

Away hotkeys:
Retained - q
Stoppage - w
TO - e
Goal - r
Behind - t
End Chain - y

## Visual Output tab

### Filters
There are 2 filters available in the Visual Output tab: Analysis Type and Filter output by Quarter/Half/Match.

#### Analysis Type
There are currently 5 types of visual analysis available: 
Inside 50 Heatmaps - create a heatmap of of Inside 50 targets.
Inside 50 Kicks - combine the location of the origin and target of kicks going inside Forward 50.
D50 Exit Kicks - combine the location of the origin and target of kicks exiting the Defensive 50.
D1/2 Set Disposals - normalise the origin of all defensive half disposals from a Set (Mark or Free Kick) and create a vector of kick length and direction.
Full Chain - follow the path of a full chain from origin to end.

Each of these analysis types works with the x, y locations and event tags created in the data collection tab.  Currently, the Goal and Behind tags are used in the Inside 50 Heatmaps; Retained, Stoppage and TO tags are used in the Inside 50 Kicks, D50 Exit Kicks and D1/2 Set Disposals outputs.

#### Filter output by Quarter/Half/Match
This dropdown provides the user with the ability to filter the visual output by the selected timeframe.

### Home Team Visual Output
The Home team visual output combines the Analysis Type and time frame filters to produce a visual representation of the collected data for the home team.

### Away Team Visual Output
The Away team visual output combines the Analysis Type and time frame filters to produce a visual representation of the collected data for the away team.

#### Save PNG
Both the Home team and Away team visual outputs have a Save PNG button located next to the plots.  This will save the current plot in PNG format to the current working folder.

## Data Output tab
The Data Output tab displays the raw collected data for both Home and Away teams in table form.

### ID to delete
This numeric input is available for both the Home and Away tables.  It allows the user to delete an ID from the table if data has been entered incorrectly.  The Delete button located underneath this input will delete the selected ID.

### Save to CSV
This button will save the current table in csv format to the current working directory.
