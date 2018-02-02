select run_date, ghc_version, Status, N,
	format('%s %%', round(100 * N / (sum(N) over (partition by run_date, ghc_version)), 2)) as Pct
from 
	(select run_date, ghc_version,
			case when build_status = 'OK' then 'OK'	else 'FAIL' end Status,
	 		count(*) as N
	from hackage.build_status
	group by run_date, ghc_version, Status) BS
where ghc_version='8.2'
order by run_date, ghc_version, Status;
