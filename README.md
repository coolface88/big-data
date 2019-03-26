# Problem 1. Extract information from the big file
A client gives us around 100000 .csv files with each of file size around 1GB to 5GB that contains the
logs of messages that are sent to 100 million users. The file has the following column format:
phone_number|message_id|send_Date|message_content
Question: Provide your ideas/solutions and implement on how to extract information of the customer
with the phone_number 0981234567.

### Solution:
I will stream those CSV files so that we are only holding small amount of data in the memory.
The lazy evaluation feature of Haskell is particular suitable for dealing with infinite data on limited resources.
In this case, I will use streaming Conduit library making explicit evaluation-as-need to this problem. 
However, it is still easy to inadvertently build up "thunks" which might fill up all memory space.
We need to use strict operations properly when handling CSV chunks. I use csv-conduit for this computation.
The Conduit library is modeled as a monad transformer which nicely combine multiple monads as once.

### Build and Exec:
- stack tool installation:

curl -sSL https://get.haskellstack.org/ | sh

for more info, visit https://docs.haskellstack.org/en/stable/README/

- download git project: 

git clone https://github.com/coolface88/big-data.git

- build:

stack build

- execute:

stack exec sol1-exe csv_file_directory_argument phone_num_argument

- example: 

stack exec sol1-exe . 0979895645

- output:

looks for all-file.csv file under current test directory 

### Note:
- CSV files should be encoded in UTF8. 
- Those CSV files should be saved with "ensure file end with a line break" activated in the text editor.
  This is needed to prevent incorrect parsing for the last line of multiple files.
  If you are not sure about those, run the following bash script to add a line break at the end to those files.
  
  for x in *; do if [ -n "$(tail -c 1 <"$x")" ]; then echo >>"$x"; fi; done
