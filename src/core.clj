(ns attic-mon.core
  (:import [oshi.software.os OperatingSystem$ProcessSort]))

(def everything-conf
  [[:os
    [:filesystem
     [:open-file-descriptors]
     [:max-file-descriptors]
     [:file-stores
      [:usable-space]
      [:total-space]
      [:logical-volume]
      [:name]
      [:description]
      [:type]
      [:mount]
      [:volume]]]
    [:network-params
     [:hostname]
     [:ipv4-gateway]
     [:dns-servers]
     [:ipv6-gateway]
     [:domain-name]]
    [:processes
     [:thread-count]
     [:process-count]
     [:processes
      [:name]
      [:process-id]
      [:up-time]
      [:kernel-time]
      [:user-time]
      [:resident-set-size]
      [:virtual-size]]]]
   [:hardware
    [:system
     [:manufacturer]
     [:model]
     [:firmware
      [:manufacturer]
      [:name]
      [:description]
      [:version]
      [:release-date]]
     [:baseboard
      [:manufacturer]
      [:model]
      [:version]
      [:serial-number]]]
    [:processor
     [:package-count]
     [:cpu-load]
     [:physical-processor-count]
     [:logical-processor-count]
     [:identifier]
     [:processor-id]]
    [:memory
     [:available]
     [:swap-used]
     [:swap-total]
     [:total]]
    [:sensors
     [:cpu-temp]
     [:fan-speeds]
     [:cpu-voltage]]
    [:power-sources
     [:time-remaining]]
    [:disks
     [:name]
     [:size]
     [:reads]
     [:writes]
     [:transfer-time]
     [:partitions
      [:name]
      [:identifier]
      [:type]
      [:major]
      [:minor]
      [:size]
      [:mount-point]]]
    [:network-interfaces
     [:name]
     [:mac-address]
     [:mtu]
     [:speed]
     [:ipv4-address]
     [:ipv6-address]
     [:packets-received]
     [:bytes-received]
     [:in-errors]
     [:packets-sent]
     [:bytes-sent]
     [:out-errors]]
    [:displays]
    [:usb-devices]]])

(def ^:private possible-kws (set (flatten everything-conf)))

(defn- generate-map [config lookup-table & [opts]]
  (reduce (fn [a b]
            (let [kw (first b)]
              (assoc a kw ((lookup-table kw)
                           {:kw     kw
                            :params (rest b)
                            :opts   opts}))))
          {}
          config))

(defn- get-filesystem-filestores [{:keys [opts params]}]
  (let [lookup-table {:usable-space   #(.getUsableSpace (get-in % [:opts :fs]))
                      :total-space    #(.getTotalSpace (get-in % [:opts :fs]))
                      :logical-volume #(.getLogicalVolume (get-in % [:opts :fs]))
                      :name           #(.getName (get-in % [:opts :fs]))
                      :description    #(.getDescription (get-in % [:opts :fs]))
                      :type           #(.getType (get-in % [:opts :fs]))
                      :mount          #(.getMount (get-in % [:opts :fs]))
                      :volume         #(.getVolume (get-in % [:opts :fs])) }]
    (mapv (fn [fs]
            (generate-map params lookup-table {:fs fs}))
          (.getFileStores (:fs opts)))))

(defn- get-filesystem [{:keys [opts params]}]
  (when (not-empty params)
    (let [filesystem   (.getFileSystem (:os opts))
          lookup-table {:open-file-descriptors #(.getOpenFileDescriptors (get-in % [:opts :fs]))
                        :max-file-descriptors  #(.getMaxFileDescriptors (get-in % [:opts :fs]))
                        :file-stores           get-filesystem-filestores}]
      (generate-map params lookup-table {:fs filesystem}))))

(defn- get-network-params [{:keys [opts params]}]
  (when (not-empty params)
    (let [net-params   (.getNetworkParams (:os opts))
          lookup-table {:hostname     #(.getHostName (get-in % [:opts :net-params]))
                        :domain-name  #(.getDomainName (get-in % [:opts :net-params]))
                        :dns-servers  #(.getDnsServers (get-in % [:opts :net-params]))
                        :ipv4-gateway #(.getIpv4DefaultGateway (get-in % [:opts :net-params]))
                        :ipv6-gateway #(.getIpv6DefaultGateway (get-in % [:opts :net-params])) }]
      (generate-map params lookup-table {:net-params net-params}))))

(defn- get-processes-processes [{:keys [opts params]}]
  (when (not-empty params)
    (let [os           (:os opts)
          lookup-table {:name              #(.getName (get-in % [:opts :process]))
                        :process-id        #(.getProcessID (get-in % [:opts :process]))
                        :up-time           #(.getUpTime (get-in % [:opts :process]))
                        :kernel-time       #(.getKernelTime (get-in % [:opts :process]))
                        :user-time         #(.getUserTime (get-in % [:opts :process]))
                        :resident-set-size #(.getResidentSetSize (get-in % [:opts :process]))
                        :virtual-size      #(.getVirtualSize  (get-in % [:opts :process]))}]
      (mapv (fn [process]
              (generate-map params lookup-table {:process process}))
            (.getProcesses os 10 OperatingSystem$ProcessSort/CPU)))))

(defn- get-processes [{:keys [opts params]}]
  (when (not-empty params)
    (let [os           (:os opts)
          lookup-table {:process-count #(.getProcessCount (get-in % [:opts :os]))
                        :thread-count  #(.getThreadCount (get-in % [:opts :os]))
                        :processes     get-processes-processes}]
      (generate-map params lookup-table {:os os}))))

(defn- get-os [{:keys [opts params]}]
  (when (not-empty params)
    (let [os           (.getOperatingSystem (:sys-info opts))
          lookup-table {:filesystem     get-filesystem
                        :network-params get-network-params
                        :processes      get-processes}]
      (generate-map params lookup-table {:os os}))))

(defn- get-computer-system-firmware [{:keys [opts params]}]
  (when (not-empty params)
    (let [fw           (.getFirmware (:sys opts))
          lookup-table {:manufacturer #(.getManufacturer (get-in % [:opts :fw]))
                        :name         #(.getName (get-in % [:opts :fw]))
                        :description  #(.getDescription (get-in % [:opts :fw]))
                        :version      #(.getVersion (get-in % [:opts :fw]))
                        :release-date #(.getReleaseDate (get-in % [:opts :fw]))}]
      (generate-map params lookup-table {:fw fw}))))

(defn- get-computer-system-baseboard [{:keys [opts params]}]
  (when (not-empty params)
    (let [bb           (.getBaseboard (:sys opts))
          lookup-table {:manufacturer  #(.getManufacturer (get-in % [:opts :bb]))
                        :model         #(.getModel (get-in % [:opts :bb]))
                        :version       #(.getVersion (get-in % [:opts :bb]))
                        :serial-number #(.getSerialNumber (get-in % [:opts :bb])) }]
      (generate-map params lookup-table {:bb bb}))))

(defn- get-computer-system [{:keys [opts params] :as m}]
  (when (not-empty params)
    (let [sys          (.getComputerSystem (:hw opts))
          lookup-table {:manufacturer  #(.getManufacturer (get-in % [:opts :sys]))
                        :model         #(.getModel (get-in % [:opts :sys]))
                        :serial-number #(.getSerialNumber (get-in % [:opts :sys]))
                        :firmware      get-computer-system-firmware
                        :baseboard     get-computer-system-baseboard}]
      (generate-map params lookup-table {:sys sys}))))

(defn- get-processor [{:keys [opts params] :as m}]
  (when (not-empty params)
    (let [proc         (.getProcessor (:hw opts))
          lookup-table {:package-count            #(.getPhysicalPackageCount (get-in % [:opts :proc]))
                        :cpu-load                 #(.getSystemCpuLoad (get-in % [:opts :proc]))
                        :physical-processor-count #(.getPhysicalProcessorCount  (get-in % [:opts :proc]))
                        :logical-processor-count  #(.getLogicalProcessorCount  (get-in % [:opts :proc]))
                        :identifier               #(.getIdentifier (get-in % [:opts :proc]))
                        :processor-id             #(.getProcessorID (get-in % [:opts :proc]))}]
      (generate-map params lookup-table {:proc proc}))))

(defn- get-sensors [{:keys [opts params] :as m}]
  (when (not-empty params)
    (let [sensors      (.getSensors (:hw opts))
          lookup-table {:cpu-temp    #(.getCpuTemperature (get-in % [:opts :sensor]))
                        :fan-speeds  #(.getFanSpeeds (get-in % [:opts :sensor]))
                        :cpu-voltage #(.getCpuVoltage (get-in % [:opts :sensor])) }]
      (generate-map params lookup-table {:sensor sensors}))))

(defn- get-power-sources-time-remaining [{:keys [opts]}]
  (.getTimeRemaining (:source opts)))

(defn- get-power-sources [{:keys [opts params] :as m}]
  (when (not-empty params)
    (let [power-sources (.getPowerSources (:hw opts))
          lookup-table  {:time-remaining get-power-sources-time-remaining}]
      (mapv (fn [source]
              (generate-map params lookup-table {:source source}))
            power-sources))))


(defn- get-disk-partitions [{:keys [opts params]}]
  (when (not-empty params)
    (let [partitions   (.getPartitions (:disk opts))
          lookup-table {:name        #(.getName (get-in % [:opts :part]))
                        :identifier  #(.getIdentification (get-in % [:opts :part]))
                        :type        #(.getType (get-in % [:opts :part]))
                        :major       #(.getMajor (get-in % [:opts :part]))
                        :minor       #(.getMinor (get-in % [:opts :part]))
                        :size        #(.getSize (get-in % [:opts :part]))
                        :mount-point #(.getMountPoint (get-in % [:opts :part]))}]
      (mapv (fn [part]
              (generate-map params lookup-table {:part part}))
            partitions))))

(defn- get-disk-stores [{:keys [opts params]}]
  (when (not-empty params)
    (let [disk-stores  (.getDiskStores (:hw opts))
          lookup-table {:name          #(.getName (get-in % [:opts :disk]))
                        :size          #(.getSize (get-in % [:opts :disk]))
                        :reads         #(.getReads (get-in % [:opts :disk]))
                        :writes        #(.getWrites (get-in % [:opts :disk]))
                        :transfer-time #(.getTransferTime (get-in % [:opts :disk]))
                        :partitions    get-disk-partitions }]
      (mapv (fn [disk]
              (generate-map params lookup-table {:disk disk}))
            disk-stores))))

(defn- get-network-interfaces [{:keys [opts params]}]
  (when (not-empty params)
    (let [net-ifs      (.getNetworkIFs (:hw opts))
          lookup-table {:name             #(.getDisplayName (get-in % [:opts :net-if]))
                        :mac-address      #(.getMacaddr (get-in % [:opts :net-if]))
                        :mtu              #(.getMTU (get-in % [:opts :net-if]))
                        :speed            #(.getSpeed (get-in % [:opts :net-if]))
                        :ipv4-address     #(.getIPv4addr (get-in % [:opts :net-if]))
                        :ipv6-address     #(.getIPv6addr (get-in % [:opts :net-if]))
                        :packets-received #(.getPacketsRecv (get-in % [:opts :net-if]))
                        :bytes-received   #(.getBytesRecv (get-in % [:opts :net-if]))
                        :in-errors        #(.getInErrors (get-in % [:opts :net-if]))
                        :packets-sent     #(.getPacketsSent (get-in % [:opts :net-if]))
                        :bytes-sent       #(.getBytesSent (get-in % [:opts :net-if]))
                        :out-errors       #(.getOutErrors (get-in % [:opts :net-if]))}]
      (mapv (fn [net-if]
              (generate-map params lookup-table {:net-if net-if}))
            net-ifs))))

(defn- get-displays [{:keys [opts params] :as m}]
  (when (not-empty params)
    (let [displays (.getDisplays (:hw opts))]
      (mapv (fn [disp]
              (str disp))
            displays))))

(defn- get-usb-devices [{:keys [opts params]}]
  (when (not-empty params)
    (let [usb-devices (.getUsbDevices (:hw opts) true)]
      (mapv (fn [dev]
              (str dev))
            usb-devices))))

(defn- get-memory [{:keys [opts params] :as m}]
  (when (not-empty params)
    (let [mem          (.getMemory (:hw opts))
          lookup-table {:available  #(.getAvailable (get-in % [:opts :mem]))
                        :total      #(.getTotal (get-in % [:opts :mem]))
                        :swap-used  #(.getSwapUsed (get-in % [:opts :mem]))
                        :swap-total #(.getSwapTotal (get-in % [:opts :mem]))}]
      (generate-map params lookup-table {:mem mem}))))

(defn- get-hardware
  [{:keys [opts params] :as m}]
  (when (not-empty params)
    (let [hw           (.getHardware (:sys-info opts))
          lookup-table {:system             get-computer-system
                        :processor          get-processor
                        :memory             get-memory
                        :sensors            get-sensors
                        :power-sources      get-power-sources
                        :disks              get-disk-stores
                        :network-interfaces get-network-interfaces
                        :displays           get-displays
                        :usb-devices        get-usb-devices}]
      (generate-map params lookup-table {:hw hw}))))

(defn get-data [config]
  (let [sys-info (oshi.SystemInfo.)
        lookup-table {:os get-os
                      :hardware get-hardware}]
    (generate-map config lookup-table {:sys-info sys-info})))
