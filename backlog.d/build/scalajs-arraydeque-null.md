- [ ] scalajs-arraydeque-null — `java.util.ArrayDeque` on Scala.js
      (sbt-scalajs 1.22.0) answers null from `pop()` on a non-empty
      deque. Found by stack-safety-catch-up-okay2 (2026-09-25):
      push/pop of (Int, Int) pairs, 200 000 rounds with a second push
      every third one, then a drain of the 66 667 left gave 192 nulls
      (0 on JVM and Native). Reduce it to the smallest sequence (likely
      a wrap-around after a growth), check the Scala.js issue tracker
      and javalib's ArrayDeque source, then report or cite. Until then,
      cross code keeps `scala.collection.mutable.Stack`/`ArrayDeque`,
      and `git grep ArrayDeque` over shared sources should stay empty
      outside scala-jvm/.
