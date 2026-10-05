- [ ] jpms-module-layout — JPMS Stage C, after okay-watch's strict headless
      profile (specs/jpms-deployment.md there). Give each okay2 module a
      distinct package, add module-info, and declare handlers with
      uses/provides. Scope the compatibility/migration contract first:
      existing okay libraries share package okay and cannot be separate
      named or automatic modules. Their one-assembly deployment is not a
      substitute for this module layout. Verify absence of split packages
      and service loading on the actual module path.
