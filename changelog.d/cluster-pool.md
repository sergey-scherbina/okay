## cluster-pool - the engine on cluster managers, specified

specs/cluster-pool.md answers the operator's ask of 2026-09-23: run
and drive okay's distributed engine on Kubernetes, in a cloud, and
everywhere Spark and Flink run, with something of our own. The answer
is a POOL — N copies of one process, kept alive by the manager,
discovered by its own DNS or a list, any of which takes a submission
(a job by name and Schema'd parameters over HTTP) and coordinates it.
No manager-specific code in the engine; per manager the cost is a
rendering in okay-deploy (`Need.Peers`) and, sometimes, a `Discovery`
source. Spark's per-application executor launch, its scheduler
backends and its jar shipping are not needed because nothing here
ships a closure. Targets: cluster, laptop, host, aws, fly, nomad,
yarn, slurm, swarm, batch; gcp/azure/render/railway refused by name;
Mesos a trigger (retired upstream). Seven stages; stage 1
(cluster-pool-process) is in the sprint queue, stages 2–6 in the
backlog under okay-cluster / dataflow. Stage 2's kind harness is what
dataflow stage 12 ("the network") was blocked on.
