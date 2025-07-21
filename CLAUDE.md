## Development Notes

- Use gmake consistently when on FreeBSD
- Use --trailer for co-author and reviewer attribution
- Use conventional commits
- After each commit add a git note for any prompts used in the work on the changeset
- Clean up the grammar and spelling to assist with rebuilding the repo
- Add notes for the prompt, issues, clarifications, and tests
- You can find the reviewer as $USER@$HOST
- Prefer to work in the project root unless working on an experiment
- All scripts should assume they can be run in cron, an experiment, etc so much check the base location and infer the project root from the script location

## Commit Strategy

**IMPORTANT**: Commit incrementally as you work. If you don't commit incrementally with notes, recovering requires a huge workflow:
- Reset working directory
- Create diffs of all changes
- Incrementally rebuild features
- Retest everything
- Recreate proper commit history

Example: When adding both integration tests and Lean tests, these should typically be separate commits with their own notes. However, if work has progressed without commits, it's better to combine related changes than lose work.