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
  
 # Problem 4. System Design
Given multiple on-premise server clusters (20-50 physical servers in each cluster) in different
locations (Vietnam, the Philippines, Singapore, India etc.). These clusters are mission critical
and run independently. Our customers will be served base on their location (customers is in
Vietnam that will be routed into the cluster in Vietnam location)
Question: Propose solutions to deploy, configure, monitor, manage these servers in one place
efficiently with high ability.

### Solution:
At the global level, we need a Geo DNS service having a logic on classifying region actual clusters. 
We might want to use Geo IP service combined with the Geo DNS service for more accurate routing. 
However, this might add up latency for the initial requests. So I will consider the tradeoffs very carefully.
There are many paid services providing those features on the markets. I can list out some here like Amazon Route 53
Google Cloud DNS and more. We could also implement our own service with the equivalent feature, but considering time and cost that might be a no go. Besides those geo-related core features, we need to monitor our clusters as a whole are up or not, so heartbeat mechanism is very important to maintain the availability. A very simple design I would come up is implementing a heartbeat service resided on each on-premise cluster. It will periodically ping-pong a signal with the global service. If there is any heartbeat missing, we need a strategy for some scenarios of failover.
  
  At the regional cluster level, the requirement here is high availability. The most practical approach for this is the implementing
  of replications. So any services running on a cluster want to have the high availability property will implement a replication at some 
  degree. Talking a bit deeper in how to design system like that. From the perspective of a service design, we will replicate its computation
  and data. At this point, the discussion about stateful or stateless and execution models are the key points for deciding frameworks,
  programming paradigms. I will make a simple cluster design fulfilling the above requirement by listing main services and their features. 
  The system needs a discovery/registry service which is a local DNS alike for the simple of taste. A consensus system, for example
  Zookeeper, etcd or Kafka, this system maintains the state consistency amongst replications for services. An API gateway service is realized by 
  a reverse proxy server or an ingress controller. Some cluster management frameworks to consider are Kubernetes or Mesosphere. Some execution models
  to consider are actor model (Orleans, Erlang OTP, Java Akka) service mesh and service fabric. 
