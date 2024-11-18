
## 待完成

- [ ] 首列缩进问题
- [ ] 首行路径的处理上


## 树状图风格示例

### A类

```bash
project_dir: ./Model
 - src
   - model
     - bridge
       - bridge_modals.m
       - bridge_updates.m
     - track
     - load
       - mov_load.m
       - vehicle_load.m
   - data
     - visual
       - real_time_present.m
   - control
     - status
       - load_status.m
       - load_partition.m
   - solver
     - newmark_solver.m
     - zhai_int_solver.m
   - interface
     - bridge_io.m
 -  test
    - load_test
      - test_main.m
    - solver_test
      - test_solver.m
 - main.m
 - README.md
 ```

### B类

 ```markdown
 project_dir: ./Model
 ├── src
 │   ├── model
 │   │   ├── bridge
 │   │   │   ├── bridge_modals.m
 │   │   │   └── bridge_updates.m
 │   │   ├── track
 │   │   └── load
 │   │       ├── mov_load.m
 │   │       └── vehicle_load.m
 │   ├── data
 │   │   └── visual
 │   │       └── real_time_present.m
 │   ├── control
 │   │   └── status
 │   │       ├── load_status.m
 │   │       └── load_partition.m
 │   ├── solver
 │   │   ├── newmark_solver.m
 │   │   └── zhai_int_solver.m
 │   └── interface
 │       └── bridge_io.m
 ├── test
 │   ├── load_test
 │   │   └── test_main.m
 │   └── solver_test
 │       └── test_solver.m
 ├── main.m
 └── README.md
```

### C类

```markdown
 project_dir: ./Model
 ┣ src
 ┃ ┣ model
 ┃ ┃ ┣ bridge
 ┃ ┃ ┃ ┣ bridge_modals.m
 ┃ ┃ ┃ ┗ bridge_updates.m
 ┃ ┃ ┣ track
 ┃ ┃ ┗ load
 ┃ ┃   ┣ mov_load.m
 ┃ ┃   ┗ vehicle_load.m
 ┃ ┣ data
 ┃ ┃ ┗ visual
 ┃ ┃   ┗ real_time_present.m
 ┃ ┣ control
 ┃ ┃ ┗ status
 ┃ ┃   ┣ load_status.m
 ┃ ┃   ┗ load_partition.m
 ┃ ┣ solver
 ┃ ┃ ┣ newmark_solver.m
 ┃ ┃ ┗ zhai_int_solver.m
 ┃ ┗ interface
 ┃   ┗ bridge_io.m
 ┣ test
 ┃ ┣ load_test
 ┃ ┃ ┗ test_main.m
 ┃ ┗ solver_test
 ┃   ┗ test_solver.m
 ┣ main.m
 ┗ README.md
```