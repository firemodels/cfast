# Running CFAST Validation Cases Manually

You can run the CFAST validation cases manually; that is, outside of CFASTbot, by executing the bash script `Run_CFAST_Cases.sh` in Windows Powershell, linux, or macOS. 
```
./Run_CFAST_Cases.sh -q batch
```
Here, `batch` is the name of the queue, assuming Slurm is installed. For Windows, where there is typically not a queuing system installed, run the cases serially like this
```
./Run_CFAST_Cases.sh -q terminal
```

