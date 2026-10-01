## check-citations-parked-history - a commit on a parked branch is preserved, not only its tip

`scripts/check-citations.sh` exempted a cited sha only when it was the
TIP of a local branch; a parked backlog item citing two probes of one
branch (frames-array-stack: its first commit and its tip) then failed the
check for every lane trying to land. The exemption is now "contained in a
local branch" (`git branch --contains`). The failure the script exists to
catch — a sha a rebase threw away — is on no branch, and still fails:
checked with a throwaway commit made by `commit-tree` and cited.
