### short

- expiry < today protection doesn't handle integers

- abstract cache API code
- demo to test spawning cache item
- demo to test full cache api

### thoughts

- [all] remove cowboy handlers ?

### done

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