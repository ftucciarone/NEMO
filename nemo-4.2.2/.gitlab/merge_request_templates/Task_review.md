#### Tests

Once the development is done, the PI should complete the tests section below and after ask the reviewers to start their review.  
This part should contain the detailed results of ~SETTE tests (restartability and reproducibility for each of the reference configuration) and detailed results of restartability and reproducibility when the option is activated on specified configurations used for this test.

**Regular checks**

- [ ] Can this change be shown to produce expected impact (option activated)?
- [ ] Can this change be shown to have a null impact (option not activated)?
- [ ] Results of the required bit comparability tests been run: are there no differences when activating the development?
- [ ] If some differences appear, is reason for the change valid/understood?
- [ ] If some differences appear, is the impact as expected on model configurations?
- [ ] Is this change expected to preserve all diagnostics?  
  - [ ] If no, is reason for the change valid/understood?
- [ ] Are there significant changes in run time/memory?

#### Review

A successful review is needed to schedule the merge of this development into the future NEMO release during next Merge Party (usually in November).

**Assessments**

- [ ] Is the proposed methodology now implemented?
- [ ] Are the code changes in agreement with the flowchart defined at preview step?
- [ ] Are the code changes in agreement with list of routines and variables as proposed at preview step?  
  - [ ] If, not, are the discrepancies acceptable?
- [ ] Is the in-line documentation accurate and sufficient?
- [ ] Do the code changes comply with NEMO coding standards?
- [ ] Is the development documented with sufficient details for others to understand the impact of the change?
- [ ] Is the project ~doc (manual, guide, web, ...) now updated or completed following the proposed summary in preview section?
