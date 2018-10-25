### short

- [all] remove GS_GLOBAL_TIMEOUT

### thoughts

- [all] remove cowboy handlers ?

### done

- top level api
- improve test layout
- demo to test full cache api
- demo to test spawning cache item
- lager logging
- abstract cache API code
- expiry < today protection doesn't handle integers
- add cache protection in case expiry < today
- add expiry to add, set
- complete cache api code
- replace cache api refs to Pid with registry(Id)
- cache naming code
- cache item spawn code
- spawn item sup with registry
- github project
- add cache_item expiry calculations
- add timedelta to wol_datetime
- add cache DOWN/EXIT handlers
- add cache level API