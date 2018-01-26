# Clue

Clue is metrics and key performance indicators (KPI) application.  


## Inspiration

The library implement metrics using concept of atomic counter update (e.g. `ets:update_counter/3`). This approach releases us from trouble of read-and-write cycles, having a background processes to aggregates the results, etc. This approach allows to sample tens of millions measurements per seconds without significant overhead. In contracts, this approach do not allow to implement rich metrics as other library provides.   


## Key features and metrics

* **Gauge** is a single value metric, the update replaces old value.
* **Counter** is an incremental single value metric, the update either increments or decrements the previous value.
* **Meter** measures a mean rate of a counter.
* **Exponential Weighted Meter** measures a mean rate of a counter and smooth it using exponentially weighted moving averages.
* **Decay** is an incremental single value metric with exponential decay smoothing

 