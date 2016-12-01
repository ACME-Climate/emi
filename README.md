# EMI
External Model Interface (EMI) is for coupling the ALM (ACME Land Model) 
with external models (EMs) such as SBeTR, VSFM, FATES, PFLOTRAN, etc.


## Science requirements for EMI
1. Support mulitple EMs.
2. Exchange data between ALM and EM at ALM's various subgrid hierarchy structure (G/T/L/C/P – level).
3. Ability to call an EM multiple times within a single ALM time integration loop.


## Overview

The schematic representation of EMI is as follows:

```
                 --------------------------------> ALM: *StateType.F90 and *FluxType.F90
                /                                 /|\
               /                                   |
              /             ___                    |
             /             │   │-------------------|
            /              │ E │------------------------>  BeTR_Public_API_For_ALM
ALM Physics -------------> │ M │------------------------>  FATES_Public_API_For_ALM
                           │ I │------------------------>  PFLOTRAN_Public_API_For_ALM
                           │   │------------------------>  VSFM_Public_API_For_ALM
                            ---
```

- EMI will have access to
  - ALM's derived types (e.g. `waterstate_vars`, `waterflux_vars`), and
  - Instances of EMs.

- EMs will not have access to ALM's derived types.

- ALM and EMs will exchange data (`emi_data`) that has following attributes:
  - `id` : Unique integer identifier
  - `num_em_stages` : Number of EM stages in which the data is exchanged
  - `em_stage_ids(:)` : Array containing IDs of EM stages in which the data is exchanged

## Workflow

1. During the initialization stage, each EM populates data list of variables it will exhcange with ALM.
  - `l2e_init_list` : Data passed from ALM to EM during initialization.
  - `e2l_init_list` : Data passed from EM to ALM during time integration.
  - `l2e_list` : Data passed from ALM to EM during time integration.
  - `e2l_list` : Data passed from EM to ALM during time integration.

2. Time integration loop involves the following:
  - If needed, preform pre-processing of data.
  -  `call EMI_Driver(em_id, em_stage, ...)`
  - Set all data to be exchanged between ALM and EM for `em_stage` to zero.
  - Pack data that is to be sent from ALM to EM for `em_stage`.
  - Verify all data needed by EM for `em_stage` was packed.
  - Call the EM (e.g. `call EM_FATES_Solve(em_stage, ..., l2e_list, e2l_list)`).
  - Unpack data sent by EM for `em_stage`.
  - Verify all data returned by EM for `em_stage` was unpacked.
