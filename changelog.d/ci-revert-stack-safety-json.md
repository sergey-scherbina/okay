## ci-revert-stack-safety-json - reverted by ci-runner

Culprit `ad34a6d119568f8bb27792688bc598dad8433067` ("stack-safety-json: the strict JSON reader skips unknown fields by a loop past the threshold (StackOverflowError at 200k unknown fields 40 deep, red first in okay and okay2); JSON rows in both inventories marked BOUNDED; spec stage 2a", lane `stack-safety-json`) failed the
whole-build gate over `20809671bddf26b68ad34d0d7e0efaaaaac9fb1a..46c801a7c518d817b2bdbd683db7bf358c8e76b7`. Reverted so master stays
something the next lane can rebase onto; re-land with the fix. Runner
log: `/Users/sergiy/work/my/okay/.work/ci/log/20260925T101539Z-20809671bddf26b68ad34d0d7e0efaaaaac9fb1a..46c801a7c518d817b2bdbd683db7bf358c8e76b7.log`.
