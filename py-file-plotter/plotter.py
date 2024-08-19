import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import pandas as pd
import time
from datetime import datetime

# File to be monitored
file_path = '../data/GEMS_tests/SerialCapture/GEMS_full_SMURP_NF0_2024-04-22.txt'

# Dictionary to hold data for each mass
data_dict = {}

# Function to parse the line of data
def parse_line(line):
    parts = line.split(',')
    hour, minute, second, month, day, year, mass, value = map(int, parts)
    timestamp = datetime(year, month, day, hour, minute, second)
    return timestamp, mass, value

# Function to update the plot
def update_plot():
    plt.clf()
    
    for mass, df in data_dict.items():
        plt.plot(df['Timestamp'], df['Value'], label=f'Mass {mass}')
    
    plt.gcf().autofmt_xdate()
    plt.gca().xaxis.set_major_formatter(mdates.DateFormatter('%H:%M:%S'))
    plt.xlabel('Time')
    plt.ylabel('Value')
    plt.yscale('log')
    plt.title('Real-time Data Streaming')
    plt.legend()
    plt.draw()
    plt.pause(0.01)

# Function to read the entire file and plot initial data
def initial_plot(file_path):
    with open(file_path, 'r') as file:
        for line in file:
            if line:
                try:
                    timestamp, mass, value = parse_line(line.strip())
                    
                    if mass not in data_dict:
                        data_dict[mass] = pd.DataFrame(columns=['Timestamp', 'Value'])
                    
                    # Append the new data to the DataFrame
                    new_data = pd.DataFrame({'Timestamp': [timestamp], 'Value': [value]})
                    data_dict[mass] = pd.concat([data_dict[mass], new_data], ignore_index=True)
                    
                except ValueError as e:
                    pass
                    #print(f"Failed to parse line: {line.strip()} with error {e}")  # Debugging: Check parsing errors
    
    update_plot()  # Plot the data after reading the entire file

# Main loop to monitor file and update plot
def monitor_file(file_path):
    with open(file_path, 'r') as file:
        file.seek(0, 2)  # Move the cursor to the end of the file
        try:
            while True:
                line = file.readline()
                if line:
                    try:
                        timestamp, mass, value = parse_line(line.strip())
                        print(f"Read line: {line.strip()}")  # Debugging: Check the raw line read
                        print(f"Parsed data - Timestamp: {timestamp}, Mass: {mass}, Value: {value}")  # Debugging: Check parsed data

                        
                        if mass not in data_dict:
                            data_dict[mass] = pd.DataFrame(columns=['Timestamp', 'Value'])
                        
                        # Append the new data to the DataFrame
                        new_data = pd.DataFrame({'Timestamp': [timestamp], 'Value': [value]})
                        data_dict[mass] = pd.concat([data_dict[mass], new_data], ignore_index=True)
                        
                    except ValueError as e:
                        print(f"Failed to parse line: {line.strip()} with error {e}")  # Debugging: Check parsing errors
                        # pass  # Skip lines that don't conform to the expected format
                
                # Update the plot every 5 seconds
                time.sleep(2)
                update_plot()
                
                # Exit if the plot window is closed
                if not plt.fignum_exists(1):
                    print("Plot window closed. Exiting...")
                    break

        except KeyboardInterrupt:
            print("Exiting gracefully...")

        finally:
            plt.close('all')  # Close the plot window
            exit(0)

if __name__ == "__main__":
    plt.ion()  # Enable interactive mode for real-time plotting
    initial_plot(file_path)
    monitor_file(file_path)

