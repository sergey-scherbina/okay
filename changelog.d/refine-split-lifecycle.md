## refine-split-lifecycle - what out(lane) holds and costs, in okay and okay2

A stream split's lane is read ONCE: a second run of a lane (or of the
rejects) fails by name — before, it was a second reader of the same
channel, silently splitting the elements. `Bulk.uncache` (a default
no-op; both `SparkBulk`s unpersist) and `Split.release()` let the
persisted tagging go. A Vector's lanes are grouped once, so `out(lane)`
is a lookup. Tests in TestRoutes and TestSparkRoutes of both cores
(release drops Spark's persisted RDD count by one).
