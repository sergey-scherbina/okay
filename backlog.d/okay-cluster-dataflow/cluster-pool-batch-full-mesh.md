- [ ] cluster-pool-batch-full-mesh — named while landing
      cluster-pool-other-managers (2026-09-24), not built: AWS Batch's
      own multi-node parallel job API hands every node
      `AWS_BATCH_JOB_MAIN_NODE_PRIVATE_IPV4_ADDRESS`, `AWS_BATCH_JOB_
      NODE_INDEX` and `AWS_BATCH_JOB_NUM_NODES` — the coordinator's
      address and the node count, never a full peer list. A pool on
      Batch today can find the main node but not every OTHER member.
      Closing it needs something Batch itself does not offer: each
      node registering its own address somewhere every other node can
      read (a shared EFS file the launch script writes and polls, or a
      DynamoDB table) before `okay-pool` starts. Trigger: a real
      workload asks to run a pool on AWS Batch specifically, rather
      than ECS (which `aws`'s existing Cloud Map rendering already
      serves).
