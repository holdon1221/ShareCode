### A Pluto.jl notebook ###
# v0.15.1

using Markdown
using InteractiveUtils

# ╔═╡ 28bf9277-ce4c-4fc0-abe8-65a7ad83685b
begin
	using Agents # Main package
	using DrWatson: @dict # Easy made Dict
	using Distributions # For infectious period distributions
	using LinearAlgebra
	using GLMakie
	using InteractiveDynamics
end

# ╔═╡ 59b3ba48-e355-48e5-8f4e-a6cbd6821f61
begin
	using JSServe
	port = 8449 # the port you want
	JSServe.JSSERVE_CONFIGURATION.listen_port[] = port 
	JSServe.JSSERVE_CONFIGURATION.external_url[] = "http://localhost:$(port)"
	JSServe.JSSERVE_CONFIGURATION.content_delivery_url[] = "http://localhost:$(port)"
	JSServe.Page(exportable=true) # needs to get displayed by Pluto
end

# ╔═╡ be0e40d1-b744-42ae-82c5-b834cffc4436
html"<button onclick='present()'>present</button>"

# ╔═╡ f145d340-9b31-11eb-3c29-993174d1c291
md"""
# 재활병원 Agent-Based Modelling

 이 연구는 재활병원에서 사용하는 여러 감염병 확산 억제 정책들을 평가하고 최적화된 전략을 도출하기 위해 설계되었다. Agent-Based Model(ABM)을 설계하여 실제 현상의 Stochastic한 부분을 대입하고, 위 정책들의 효용성에 대해 분포를 제시하고자 한다.
"""

# ╔═╡ 8533930e-8915-4e8f-9011-a9eccc197a96
md"## 사용하는 Package 및 외부 함수 호출

`Agents.jl`을 기본 package로 사용하고, 코드들을 이용하여 모델을 만든다.
"

# ╔═╡ beeb5b68-adc9-48c6-b827-a70cbfddf9e5
md"## 상수 정의

아래 상수들을 이용한다.
"

# ╔═╡ 73472c51-067b-4053-85e5-0b0661468b43
begin
	STEPS_PER_SECOND = 1
	STEPS_PER_MINUTE = 60 * STEPS_PER_SECOND
	STEPS_PER_HOUR = 60 * STEPS_PER_MINUTE
	STEPS_PER_DAY = 24 * STEPS_PER_HOUR
	
	SECONDS, MINUTES, HOURS, DAYS = 1STEPS_PER_SECOND,1STEPS_PER_MINUTE,
								1STEPS_PER_HOUR,1STEPS_PER_DAY
end

# ╔═╡ e2b23d40-246d-44a1-ac66-04f56422f94e
md"## Agent 설정

각 Agent는 `id`와 현재 position을 의미하는 `pos`, 어떤 infectious stage(S, I, R)에 있는 지 알려주는 `status` (default = S), 언제 infect되었는 지인 `days_infected` (default = 0), 어떤 상황에 있는 지 알려주는 `occupation`, 그리고 동선인 `route` (설정이 안되어 있으면 random walking)로 이루어져 있다.

### Agent class 설정
mutable struct 로 Agents.jl에 사용하기 용이한 형태로 구성 (속도 면에서 손해가 있을 수 있다.)
"

# ╔═╡ aba2fffb-90fb-474a-a557-d5b0f7dab0fb
begin
	struct BED
		id::Int
		pos::NTuple{2,Int}
		isleft::Bool
	end
	
	function pushBED!(BEDS::Vector{BED}; pos::NTuple{2,Int}, isleft::Bool)
		maxid = max((b.id for b ∈ BEDS)...)
		
		new_bed = BED(maxid+1, pos, isleft)
		push!(BEDS, new_bed)
		return new_bed
	end
	
	function pushBED!(BEDS::Vector{BED}; id::Int, pos::NTuple{2,Int}, isleft::Bool)	
		new_bed = BED(id, pos, isleft)
		push!(BEDS, new_bed)
		return new_bed
	end
end

# ╔═╡ 99e34287-5df2-46a9-a7e8-1b709b22a449
begin
	@enum InfectiousStatus S=1 I R
	@enum Occupation Patient=1 Caregiver Doctor Nurse Para
end

# ╔═╡ b7d3a84b-d004-4556-a0d7-5ce9dae7583f
begin
	@agent Rehab_agent GridAgent{2} begin
		# variables for the infectious disease dynamics
		status::InfectiousStatus
		days_infected::Int # count dates from the infected day
		infectious_period::Int # sampled from the dist'n in model
		vulnerability::Float64
		# variables for agents-info
		occupation::Occupation
		route::Vector{NTuple{2,Int}}
		stationary_timer::Vector{Int}
	end

	function Rehab_agent(
			id::Int,
			pos::NTuple{2,Int};	
			status::InfectiousStatus=S,
			days_infected::Int=0,
			infectious_period::Int, 
			vulnerability::Float64=1.0,
			occ::Occupation,
			route::Vector{NTuple{2,Int}}=NTuple{2,Int}[],
			stationary_timer::Vector{Int}=Int[]
		)

		return Rehab_agent(id, pos, 
						   status, days_infected, infectious_period, vulnerability,
						   occ, route, stationary_timer)
	end
end

# ╔═╡ 34d3d027-2b13-461c-b92c-f5a5c41c2d41
md"""
### Methods for agents

`agent_step!`은 각 step에서 agent들이 해야하는 일들을 나타내고, model 차원에서 각 step에서 agent별로 이들을 실행하게 된다. 각 agent들은 아래 일들을 수행하게 된다.


1. `move_agent!`: Agents의 움직임은 route에 따라 정해지며, random의 경우 각 step 마다 갈 수 없는 지역(벽, 등)으로 가려하는 지 파악한다. 
2. `add_infection_date!`: Agent 별로 infected된 이후 몇 step이 지났는 지 파악하기 위해 step마다 추가한다.
3. `recover!`: 치료는 model level에서 정해지는 distribution에 의해 agent별로 정해진다.
"""

# ╔═╡ f2a53d7d-5e9f-4f94-80b6-792164e09c23
add_infection_date!(a::Rehab_agent) = (a.status == I) && (a.days_infected += 1)

# ╔═╡ 9cf4fe5a-5e92-4395-8b63-8dc7216123b3
recover!(a::Rehab_agent) = (a.status == I && a.days_infected ≥ a.infectious_period) && 
								(a.status = R)

# ╔═╡ d45a254d-bf40-45b5-8cef-0c93e2c3d69a
begin
	rj_2() = rand(((-1,0),(1,0),(0,-1),(0,1),(0,0)))
	random_jump_2D(pos) = pos .+ rj_2()

	function move_agent_rehab!(a::Rehab_agent, model)
		# the 1st element of stationary_timer is not zero ➡ don't move
		# the 1st element of stationary_timer is zero or empty ➡ move
		# If stationary_timer of a is empty, then there's no route which
		# means random move or no moves are implemented.
		if !isempty(a.stationary_timer)
			(a.stationary_timer[1] > 0) && # wait for movement 
				(a.stationary_timer[1] -= 1; return) # no movement
		end
		
		# at outside there's no movement
		if a.pos == model.outside_position
			# no movement of Patients			
			if (a.occupation == Patient)
				nothing
			end
			
			# movement of Caregivers is random place of door
			(a.occupation == Caregiver) && 
				(move_agent!(a, rand(model.door_sites), model))
			
			# movement of Doctor and Nurse is determined by route
			if a.occupation == Doctor || a.occupation == Nurse
				isempty(a.route) && return
				# deteremined door = a.route[1]
				move_agent!(a, a.route[1], model)
				
				popfirst!(a.route) # pop this place
				popfirst!(a.stationary_timer) # pop this place
			end
			
			# @TODO movement of Paramedic (not implemented)
			
		else
			# no movement of Patients
			if (a.occupation == Patient)
				nothing
			end
			
			# movement of Caregivers is randomly determined
			if a.occupation == Caregiver
				next_pos = random_jump_2D(a.pos)
				while next_pos ∈ union(model.unwalkable, model.bed_sites)
					next_pos = random_jump_2D(a.pos)
				end
				move_agent!(a, next_pos, model)
			end
			
			# movement of Doctor and Nurse is determined by route
			if a.occupation == Doctor || a.occupation == Nurse
				
				if isempty(a.route)
					out_pos = a.pos .- (0,1)
					move_agent!(a, out_pos, model)
				else					
					direction_real = normalize([x for x in a.route[1] .- a.pos])
					direction_int = tuple(Int.(direction_real)...)

					next_pos = a.pos .+ direction_int

					move_agent!(a, next_pos, model)

					if a.pos ∈ a.route
						popfirst!(a.route) # pop this place
						popfirst!(a.stationary_timer) # pop this place
					end
				end
			end
			
			# @TODO movement of Paramedic (not implemented)
			
		end
		
		# go outside 10 seconds
		if a.pos ∈ model.outside
			move_agent!(a, model.outside_position, model) # outside Think later.
			a.stationary_timer = [10, a.stationary_timer...]
		end
	end
end

# ╔═╡ ebacac04-ec0b-44e2-a44b-4459469e097c
function agent_step_method!(a, m)
	move_agent_rehab!(a,m)
	add_infection_date!(a)
	recover!(a)
end

# ╔═╡ d4f79f93-704a-47a6-b7ef-0609601ba0e0
md"""
## Model 설정

재활병원 ABM의 Model initialization method를 정의하고, 어떤 method들이 있을 지 정한다.
"""

# ╔═╡ 78ff512f-c5b9-4448-b9b0-a93270c810fb
function make_a_rectangular_room_beds_sw_ne!(
		beds::Vector{BED}, 
		around_beds::Vector{NTuple{2,Int}},
		wall_list::Vector{NTuple{2,Int}},
		sw_point::NTuple{2,Int},
		ne_point::NTuple{2,Int}
	)
	Lx = ne_point[1] - sw_point[1] + 1
	Ly = ne_point[2] - sw_point[2] + 1
	
	door_size = 2 # 2 people
	
	door_start_point = (sw_point[1] + Int(floor((Lx - door_size)/2)),
						sw_point[2])
	door_end_point = door_start_point .+ (door_size - 1, 0)

	# define the positions of beds and the things around them (6 beds in a room)
	maxid = isempty(beds) ? 0 : max((b.id for b ∈ beds)...)
	
	pushBED!(beds, id=maxid+1, pos=(sw_point[1]+1, sw_point[2]+2), isleft = true)
	pushBED!(beds, id=maxid+2, pos=(sw_point[1]+1, sw_point[2]+4), isleft = true)
	pushBED!(beds, id=maxid+3, pos=(sw_point[1]+1, sw_point[2]+6), isleft = true)
	pushBED!(beds, id=maxid+4, pos=(ne_point[1]-1, sw_point[2]+6), isleft = false)
	pushBED!(beds, id=maxid+5, pos=(ne_point[1]-1, sw_point[2]+4), isleft = false)
	pushBED!(beds, id=maxid+6, pos=(ne_point[1]-1, sw_point[2]+2), isleft = false)

	# define the grids around beds (unwalkable)
	around_beds_this_room = (
		(bs.isleft ? bs.pos .+ (1, 0) : bs.pos .- (1, 0) for bs ∈ beds)...,
		(bs.isleft ? bs.pos .+ (2, 0) : bs.pos .- (2, 0) for bs ∈ beds)...,
	)
	
	for block ∈ around_beds_this_room
		push!(around_beds, block)
	end
	
	# define the wall grids (unwalkable)
	wall_list_this_room = (
		((i, ne_point[2]) for i ∈ sw_point[1]:ne_point[1])..., # walls
		((i, sw_point[2]) for i ∈ sw_point[1]:(door_start_point[1]-1))..., # walls
		((i, sw_point[2]) for i ∈ (door_end_point[1]+1):ne_point[1])..., # walls
		((ne_point[1], i) for i ∈ sw_point[2]:ne_point[2])..., # walls
		((sw_point[1], i) for i ∈ sw_point[2]:ne_point[2])..., # walls
	)
	
	for block ∈ wall_list_this_room
		push!(wall_list, block)
	end
end

# ╔═╡ 24a1b919-71f3-48dc-9ced-6bbb489a9b8b
function rehab_model_initialization(;
		p_trans = 1e-3,
		pat_vul = 1.05,
		infectious_period_distn = Exponential(2DAYS),
		num_patients = 24,
		num_caregivers = 24,
		num_doctors = 6,
		num_nurses = 6,
		num_paras = 10
	)

	L = 9 # includes wall size
	TOTAL_LV = 2
	
	Lx = L+2
	Ly = L+1
	
	# define space as a grid
	space = GridSpace((10Lx, 10Ly); 
		periodic = false,
		metric = :euclidean,
	)
	
	outside_position = (2Lx-5, 2Ly-5)

	# define the positions of beds and the things around them (6 beds in a room)
	beds = BED[]
	around_beds = NTuple{2,Int}[]
	wall_list = NTuple{2,Int}[]
	
	# make rooms
	sw_points = collect((k,2) for k ∈ 1:(L):5L)
	ne_points = collect((k,L+1) for k ∈ (L+1):(L):6L)
	
	for k = 1:4
		make_a_rectangular_room_beds_sw_ne!(
			beds, 	
			around_beds,
			wall_list,
			sw_points[k], ne_points[k]
		)
	end
	
	bed_sites = collect(bed.pos for bed ∈ beds)
	unwalkable = (around_beds..., wall_list...)
	
	# define the door (go outside)
	door_size = 2
	door_sites = NTuple{2,Int}[]
	outside = NTuple{2,Int}[]
	
	st_pos = Int(floor((L+2 - door_size)/2))
	
	for k = 1:4
		door_start_point = (sw_points[k][1] + st_pos, sw_points[k][2])
		door_end_point = door_start_point .+ (door_size-1, 0)
	
		door_sites_i = collect(
			(i,sw_points[k][2]) for i ∈ door_start_point[1]:door_end_point[1]
			)
		outside_i = collect(
			(i,sw_points[k][2]-1) for i ∈ door_start_point[1]:door_end_point[1]
			)
		door_sites = (door_sites..., door_sites_i...)
		outside = (outside..., outside_i...)
	end

	# define model properties ≡ Dict(:name = name)
	properties = @dict(
		p_trans,
		pat_vul,
		infectious_period_distn,
		outside_position,
		door_sites,
		outside,
		bed_sites,
		wall_list,
		around_beds,
		unwalkable,
	)

	# define the model
	rehab_model = AgentBasedModel(Rehab_agent, space;
								  properties = properties,
								  scheduler = fastest)

	# set the patients at the bed
	for id_patients = 1:num_patients
		pos = beds[id_patients].pos

		add_agent!(
			pos, rehab_model;
			status = S,
			infectious_period = round(Int, rand(infectious_period_distn)),
			vulnerability = pat_vul,
			occ = Patient,
			stationary_timer = [10^10] # no movement
			)
	end

	# set the caregivers at the right above beds
	for id_caregivers = 1:num_caregivers
		pos = beds[id_caregivers].pos .+ (0, 1)

		add_agent!(
			pos, rehab_model;
			status = (id_caregivers == 1) ? I : S,
			infectious_period = round(Int, rand(infectious_period_distn)),
			occ = Caregiver
			)
	end

	# set the doctors at the outside and add routes
	for id_doctor = 1:num_doctors
		pos = outside_position
		
		entrance_door = door_sites[1]
		rounding_route = collect(
			bs.pos .+ (bs.isleft ? (3, 0) : (-3, 0)) for bs ∈ beds
			)
		exit_door = door_sites[end]
		
		route = [entrance_door, rounding_route..., exit_door]
		
		rounding_time = collect(5MINUTES for i = 1:num_patients)
		
		stationary_timer = [
			(id_doctor-1)*20MINUTES,
			0SECONDS,
			rounding_time...,
			0SECONDS,
		]

		add_agent!(
			pos, rehab_model;
			infectious_period = round(Int, rand(infectious_period_distn)),
			occ = Doctor,
			route = route,
			stationary_timer = stationary_timer
			)
	end

	# set the nurses at the outside and add routes
	for id_nurse = 1:num_nurses
		pos = outside_position		
		
		entrance_door = door_sites[1]
		rounding_route = collect(
			bs.pos .+ (bs.isleft ? (3, -1) : (-3, -1)) for bs ∈ beds
			)
		exit_door = door_sites[end]
		
		route = [entrance_door, rounding_route..., exit_door]
		
		rounding_time = collect(5MINUTES for i = 1:num_patients)
		
		stationary_timer = [
			(id_nurse-1)*20MINUTES, 
			0SECONDS,
			rounding_time..., 
			0SECONDS,
		]
		
		add_agent!(
			pos, rehab_model;
			infectious_period = round(Int,rand(infectious_period_distn)),
			occ = Nurse,
			route = route,
			stationary_timer = stationary_timer
			)
	end

	# set the para-medics at the outside
	for id_para = 1:num_paras
		pos = outside_position

		add_agent!(
			pos, rehab_model;
			infectious_period = round(Int, rand(infectious_period_distn)),
			occ = Para
			)
	end
	
	return rehab_model
end

# ╔═╡ 0b18b161-c2fa-4e6c-8975-c300d9c793f7
md"""
### Methods for model

1. `transmit!`: 감염은 model level에서 정해지는 확률, `p_trans`로 일어나고, 같은 칸에 있는 agents에서 전파된다. 
"""

# ╔═╡ 0811c42b-e0fd-4788-a886-54a5df2c5915
function transmit!(a1::Rehab_agent, a2::Rehab_agent, model)
	# If there's no infected one, nothing happens
	(count(a.status == I for a in (a1,a2)) < 1) && return
	
	# Choose who the infectious is (S<I by @enum) 
	agent_S, agent_I = sort([a1,a2], by=x->x.status)
	
	# Infect the susceptible agent (this impacts on the outside variable)
	(rand(model.rng) ≤ agent_S.vulnerability * model.p_trans) && 
		(agent_S.status = I)
end

# ╔═╡ 2c75e804-653f-4d37-b54c-cd7c60a42e91
md"""
## Data Collector 함수 설정

모델에서 다음과 같은 Data를 추출한다.

1. Number of $S, I, R$
2. Number of $S_P, I_P, R_P$ for $P$: Patients
3. Number of $S_C, I_C, R_C$ for $C$: Caregivers
4. Number of $S_D, I_D, R_D$ for $D$: Doctors
5. Number of $S_N, I_N, R_N$ for $N$: Nurses
6. Number of $S_{pr}, I_{pr}, R_{pr}$ for $pr$: Para
7. Number of incidence for each time step?
"""

# ╔═╡ 6572ad81-4c51-4bf0-8383-f9b49fade795
begin
	num_S(a) = (a.status == S)
	num_I(a) = (a.status == I)
	num_R(a) = (a.status == R)
	
	num_SP(a) = (a.status == S && a.occupation == Patient)
	num_IP(a) = (a.status == I && a.occupation == Patient)
	num_RP(a) = (a.status == R && a.occupation == Patient)
	
	num_SC(a) = (a.status == S && a.occupation == Caregiver)
	num_IC(a) = (a.status == I && a.occupation == Caregiver)
	num_RC(a) = (a.status == R && a.occupation == Caregiver)
	
	num_SD(a) = (a.status == S && a.occupation == Doctor)
	num_ID(a) = (a.status == I && a.occupation == Doctor)
	num_RD(a) = (a.status == R && a.occupation == Doctor)
	
	num_SN(a) = (a.status == S && a.occupation == Nurse)
	num_IN(a) = (a.status == I && a.occupation == Nurse)
	num_RN(a) = (a.status == R && a.occupation == Nurse)
	
	num_Spr(a) = (a.status == S && a.occupation == Para)
	num_Ipr(a) = (a.status == I && a.occupation == Para)
	num_Rpr(a) = (a.status == R && a.occupation == Para)
	
	adata = [
		(num_S, count), 	(num_I, count), 	(num_R, count),
		# (num_SP, count), 	(num_IP, count),	(num_RP, count),
		# (num_SD, count), 	(num_ID, count),	(num_RD, count),
		# (num_SN, count), 	(num_IN, count),	(num_RN, count),
		# (num_Spr, count),	(num_Ipr, count),	(num_Rpr, count)
	]
end

# ╔═╡ 3fcc753d-e261-46b6-8f31-12dab98a5bc1
md"""
## Model run과 data plot

Model을 실행시키고 Agent들의 움직임과 결과 그래프를 그림
"""

# ╔═╡ fcf6e92b-fd5b-4b11-89b6-74a88289add3
rehab_model = rehab_model_initialization()

# ╔═╡ aed9eb04-7374-4a2f-ace4-42b14d574120
function static_walls!(ax, model)
	wall_sites_x = [ws[1]-0.5 for ws ∈ model.wall_list]
	wall_sites_y = [ws[2]-0.5 for ws ∈ model.wall_list]
	
	bed_sites_x = [bs[1]-0.5 for bs ∈ model.bed_sites]
	bed_sites_y = [bs[2]-0.5 for bs ∈ model.bed_sites]
	
	around_beds_x = [ab[1]-0.5 for ab ∈ model.around_beds]
	around_beds_y = [ab[2]-0.5 for ab ∈ model.around_beds]
	
	scatter!(bed_sites_x, bed_sites_y, 
		marker = :rect,
		markersize = 10,
		color = to_color("#B0D8B8"),
		) 
	
	scatter!(around_beds_x, around_beds_y,
		marker = :rect,
		markersize = 10,
		color = to_color("#D0FFDA"),
		)
	
	scatter!(wall_sites_x, wall_sites_y, 
		marker = :rect,
		markersize = 12,
		color = :black,
		) 
end

# ╔═╡ 98693f6c-015a-4410-a386-f6e1659d6491
begin
	# set color
	ac(a) = to_color(
		(a.status == S) ? "#000000" :
		(a.status == I) ? "#F70000" : 
		"#45A42D"
		) # Recovered
	
	# set marker
	am(a) = (a.occupation == Patient) ? '♿' :
				(a.occupation == Caregiver) ? '⚲' :
				(a.occupation == Doctor) ? '𝔻' :
				(a.occupation == Nurse) ? 'ℕ' :
				'ℙ' # Para-medics
	
	# set size
	as(a) = 10
end

# ╔═╡ 23298957-3c0c-420b-a1bc-84438882db08
begin
	params = Dict(
		:p_trans => 0:0.001:1,
		:pat_vul => 1:0.01:2,
		)
end

# ╔═╡ 3e167001-1d04-4b82-8657-492e9fc4566b
 GLMakie.activate!()

# ╔═╡ 168fc810-5cf7-412a-b732-f0543ad27fd5
# InteractiveDynamics.abm_video("tmp_video2.mp4",
# 	rehab_model, agent_step_method!, model_step_method!;
# 	ac = ac, 
# 	as = as, 
# 	am = am, 
# 	scatterkwargs = (
# 		showaxis = false,
# 		minorgrid = true,
# 		),
# 	static_preplot! = static_wazlls!,
# 	framerate = 1HOURS,
# 	frames = 3DAYS,
# );

# ╔═╡ bb2f5853-bc46-4171-a2c0-ee8abf90282c
begin
	struct PairIterator{A}
		pairs::Vector{Tuple{Int,Int}}
		agents::Dict{Int,A}
	end
	
	Base.length(iter::PairIterator) = length(iter.pairs)
	
	function Base.iterate(iter::PairIterator, i=1)
		(i > length(iter)) && return nothing
		
		p = iter.pairs[i]
		id1, id2 = p
		
		return (iter.agents[id1], iter.agents[id2]), i+1
	end
end

# ╔═╡ acbc5227-8e58-4583-924c-a36dd32b3f56
function all_pairs_grid(
		model::ABM{<:GridSpace},
		r::Real,
		exact = true,
		)
	pairs = Tuple{Int, Int}[]
	
	for a ∈ allagents(model)
		if a.pos == model.outside_position
			continue;
		end
		for nid ∈ nearby_ids(a, model, r)
			# sort by id
			new_pair = isless(a.id, nid) ? (a.id, nid) : (nid, a.id)
			new_pair ∉ pairs && push!(pairs, new_pair)
		end
	end
	
	return PairIterator(pairs, model.agents)
end

# ╔═╡ cea0fa3d-9b9a-480e-a6b6-cda5ac870985
function model_step_method!(model)
	# transmission occurs of all agents in the same position
	transmission_radius = 1
	
	# transmission
	for (a1, a2) in all_pairs_grid(model, transmission_radius, :all)
		# transmission doesn't occure for outside agents
		(a1.pos == model.outside_position || a2.pos == model.outside_position) ? 
			continue : transmit!(a1, a2, model)
	end
end

# ╔═╡ 25498ab7-4dd4-4074-83c9-cc71bfdc9794
abm_data_exploration(#"tmp_video.mp4",
	rehab_model, agent_step_method!, model_step_method!, params;
	ac = ac, 
	as = as, 
	am = am, 
	scatterkwargs = (
		showaxis = false,
		minorgrid = true,
		),
	static_preplot! = static_walls!,
	adata,
);

# ╔═╡ 00000000-0000-0000-0000-000000000001
PLUTO_PROJECT_TOML_CONTENTS = """
[deps]
Agents = "46ada45e-f475-11e8-01d0-f70cc89e6671"
Distributions = "31c24e10-a181-5473-b8eb-7969acd0382f"
DrWatson = "634d3b9d-ee7a-5ddf-bec9-22491ea816e1"
GLMakie = "e9467ef8-e4e7-5192-8a1a-b1aee30e663a"
InteractiveDynamics = "ec714cd0-5f51-11eb-0b6e-452e7367ff84"
JSServe = "824d6782-a2ef-11e9-3a09-e5662e0c26f9"
LinearAlgebra = "37e2e46d-f89d-539d-b4ee-838fcccc9c8e"

[compat]
Agents = "~4.5.4"
Distributions = "~0.25.15"
DrWatson = "~2.3.0"
GLMakie = "~0.4.6"
InteractiveDynamics = "~0.17.2"
JSServe = "~1.2.3"
"""

# ╔═╡ 00000000-0000-0000-0000-000000000002
PLUTO_MANIFEST_TOML_CONTENTS = """
# This file is machine-generated - editing it directly is not advised

[[AbstractFFTs]]
deps = ["LinearAlgebra"]
git-tree-sha1 = "485ee0867925449198280d4af84bdb46a2a404d0"
uuid = "621f4979-c628-5d54-868e-fcf4e3e8185c"
version = "1.0.1"

[[AbstractTrees]]
git-tree-sha1 = "03e0550477d86222521d254b741d470ba17ea0b5"
uuid = "1520ce14-60c1-5f80-bbc7-55ef81b5835c"
version = "0.3.4"

[[Adapt]]
deps = ["LinearAlgebra"]
git-tree-sha1 = "84918055d15b3114ede17ac6a7182f68870c16f7"
uuid = "79e6a3ab-5dfb-504d-930d-738a2a938a0e"
version = "3.3.1"

[[Agents]]
deps = ["CSV", "DataFrames", "DataStructures", "Distributed", "GraphRecipes", "JLD2", "LightGraphs", "LinearAlgebra", "OpenStreetMapX", "ProgressMeter", "Random", "RecipesBase", "Requires", "Scratch", "StatsBase"]
git-tree-sha1 = "7cc7d221f4e00fe17ec5777950d6056a673b299c"
uuid = "46ada45e-f475-11e8-01d0-f70cc89e6671"
version = "4.5.4"

[[Animations]]
deps = ["Colors"]
git-tree-sha1 = "e81c509d2c8e49592413bfb0bb3b08150056c79d"
uuid = "27a7e980-b3e6-11e9-2bcd-0b925532e340"
version = "0.4.1"

[[ArgTools]]
uuid = "0dad84c5-d112-42e6-8d28-ef12dabb789f"

[[ArnoldiMethod]]
deps = ["LinearAlgebra", "Random", "StaticArrays"]
git-tree-sha1 = "f87e559f87a45bece9c9ed97458d3afe98b1ebb9"
uuid = "ec485272-7323-5ecc-a04f-4719b315124d"
version = "0.1.0"

[[ArrayInterface]]
deps = ["IfElse", "LinearAlgebra", "Requires", "SparseArrays", "Static"]
git-tree-sha1 = "85d03b60274807181bae7549bb22b2204b6e5a0e"
uuid = "4fba245c-0d91-5ea0-9b3e-6abc04ee57a9"
version = "3.1.30"

[[Artifacts]]
uuid = "56f22d72-fd6d-98f1-02f0-08ddc0907c33"

[[Automa]]
deps = ["Printf", "ScanByte", "TranscodingStreams"]
git-tree-sha1 = "d50976f217489ce799e366d9561d56a98a30d7fe"
uuid = "67c07d97-cdcb-5c2c-af73-a7f9c32a568b"
version = "0.8.2"

[[AxisAlgorithms]]
deps = ["LinearAlgebra", "Random", "SparseArrays", "WoodburyMatrices"]
git-tree-sha1 = "a4d07a1c313392a77042855df46c5f534076fab9"
uuid = "13072b0f-2c55-5437-9ae7-d433b7a33950"
version = "1.0.0"

[[Base64]]
uuid = "2a0f44e3-6c83-55bd-87e4-b1978d98bd5f"

[[Bzip2_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "19a35467a82e236ff51bc17a3a44b69ef35185a2"
uuid = "6e34b625-4abd-537c-b88f-471c36dfa7a0"
version = "1.0.8+0"

[[CEnum]]
git-tree-sha1 = "215a9aa4a1f23fbd05b92769fdd62559488d70e9"
uuid = "fa961155-64e5-5f13-b03f-caf6b980ea82"
version = "0.4.1"

[[CSV]]
deps = ["Dates", "Mmap", "Parsers", "PooledArrays", "SentinelArrays", "Tables", "Unicode"]
git-tree-sha1 = "b83aa3f513be680454437a0eee21001607e5d983"
uuid = "336ed68f-0bac-5ca0-87d4-7b16caf5d00b"
version = "0.8.5"

[[Cairo_jll]]
deps = ["Artifacts", "Bzip2_jll", "Fontconfig_jll", "FreeType2_jll", "Glib_jll", "JLLWrappers", "LZO_jll", "Libdl", "Pixman_jll", "Pkg", "Xorg_libXext_jll", "Xorg_libXrender_jll", "Zlib_jll", "libpng_jll"]
git-tree-sha1 = "f2202b55d816427cd385a9a4f3ffb226bee80f99"
uuid = "83423d85-b0ee-5818-9007-b63ccbeb887a"
version = "1.16.1+0"

[[ChainRulesCore]]
deps = ["Compat", "LinearAlgebra", "SparseArrays"]
git-tree-sha1 = "bdc0937269321858ab2a4f288486cb258b9a0af7"
uuid = "d360d2e6-b24c-11e9-a2a3-2a2ae2dbcce4"
version = "1.3.0"

[[CodecZlib]]
deps = ["TranscodingStreams", "Zlib_jll"]
git-tree-sha1 = "ded953804d019afa9a3f98981d99b33e3db7b6da"
uuid = "944b1d66-785c-5afd-91f1-9de20f533193"
version = "0.7.0"

[[ColorBrewer]]
deps = ["Colors", "JSON", "Test"]
git-tree-sha1 = "61c5334f33d91e570e1d0c3eb5465835242582c4"
uuid = "a2cac450-b92f-5266-8821-25eda20663c8"
version = "0.4.0"

[[ColorSchemes]]
deps = ["ColorTypes", "Colors", "FixedPointNumbers", "Random"]
git-tree-sha1 = "9995eb3977fbf67b86d0a0a0508e83017ded03f2"
uuid = "35d6a980-a343-548e-a6ea-1d62b119f2f4"
version = "3.14.0"

[[ColorTypes]]
deps = ["FixedPointNumbers", "Random"]
git-tree-sha1 = "32a2b8af383f11cbb65803883837a149d10dfe8a"
uuid = "3da002f7-5984-5a60-b8a6-cbb66c0b333f"
version = "0.10.12"

[[ColorVectorSpace]]
deps = ["ColorTypes", "Colors", "FixedPointNumbers", "LinearAlgebra", "SpecialFunctions", "Statistics", "StatsBase"]
git-tree-sha1 = "4d17724e99f357bfd32afa0a9e2dda2af31a9aea"
uuid = "c3611d14-8923-5661-9e6a-0046d554d3a4"
version = "0.8.7"

[[Colors]]
deps = ["ColorTypes", "FixedPointNumbers", "Reexport"]
git-tree-sha1 = "417b0ed7b8b838aa6ca0a87aadf1bb9eb111ce40"
uuid = "5ae59095-9a9b-59fe-a467-6f913c188581"
version = "0.12.8"

[[Compat]]
deps = ["Base64", "Dates", "DelimitedFiles", "Distributed", "InteractiveUtils", "LibGit2", "Libdl", "LinearAlgebra", "Markdown", "Mmap", "Pkg", "Printf", "REPL", "Random", "SHA", "Serialization", "SharedArrays", "Sockets", "SparseArrays", "Statistics", "Test", "UUIDs", "Unicode"]
git-tree-sha1 = "727e463cfebd0c7b999bbf3e9e7e16f254b94193"
uuid = "34da2185-b29b-5c13-b0c7-acf172513d20"
version = "3.34.0"

[[CompilerSupportLibraries_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "e66e0078-7015-5450-92f7-15fbd957f2ae"

[[Contour]]
deps = ["StaticArrays"]
git-tree-sha1 = "9f02045d934dc030edad45944ea80dbd1f0ebea7"
uuid = "d38c429a-6771-53c6-b99e-75d170b6e991"
version = "0.5.7"

[[Crayons]]
git-tree-sha1 = "3f71217b538d7aaee0b69ab47d9b7724ca8afa0d"
uuid = "a8cc5b0e-0ffa-5ad4-8c14-923d3ee1735f"
version = "4.0.4"

[[DataAPI]]
git-tree-sha1 = "ee400abb2298bd13bfc3df1c412ed228061a2385"
uuid = "9a962f9c-6df0-11e9-0e5d-c546b8b5ee8a"
version = "1.7.0"

[[DataFrames]]
deps = ["Compat", "DataAPI", "Future", "InvertedIndices", "IteratorInterfaceExtensions", "LinearAlgebra", "Markdown", "Missings", "PooledArrays", "PrettyTables", "Printf", "REPL", "Reexport", "SortingAlgorithms", "Statistics", "TableTraits", "Tables", "Unicode"]
git-tree-sha1 = "d785f42445b63fc86caa08bb9a9351008be9b765"
uuid = "a93c6f00-e57d-5684-b7b6-d8193f3e46c0"
version = "1.2.2"

[[DataStructures]]
deps = ["Compat", "InteractiveUtils", "OrderedCollections"]
git-tree-sha1 = "7d9d316f04214f7efdbb6398d545446e246eff02"
uuid = "864edb3b-99cc-5e75-8d2d-829cb0a9cfe8"
version = "0.18.10"

[[DataValueInterfaces]]
git-tree-sha1 = "bfc1187b79289637fa0ef6d4436ebdfe6905cbd6"
uuid = "e2d170a0-9d28-54be-80f0-106bbe20a464"
version = "1.0.0"

[[Dates]]
deps = ["Printf"]
uuid = "ade2ca70-3891-5945-98fb-dc099432e06a"

[[DelimitedFiles]]
deps = ["Mmap"]
uuid = "8bb1440f-4735-579b-a4ab-409b98df4dab"

[[Distributed]]
deps = ["Random", "Serialization", "Sockets"]
uuid = "8ba89e20-285c-5b6f-9357-94700520ee1b"

[[Distributions]]
deps = ["ChainRulesCore", "FillArrays", "LinearAlgebra", "PDMats", "Printf", "QuadGK", "Random", "SparseArrays", "SpecialFunctions", "Statistics", "StatsBase", "StatsFuns"]
git-tree-sha1 = "c2dbc7e0495c3f956e4615b78d03c7aa10091d0c"
uuid = "31c24e10-a181-5473-b8eb-7969acd0382f"
version = "0.25.15"

[[DocStringExtensions]]
deps = ["LibGit2"]
git-tree-sha1 = "a32185f5428d3986f47c2ab78b1f216d5e6cc96f"
uuid = "ffbed154-4ef7-542d-bbb7-c09d3a79fcae"
version = "0.8.5"

[[Downloads]]
deps = ["ArgTools", "LibCURL", "NetworkOptions"]
uuid = "f43a241f-c20a-4ad4-852c-f6b1247861c6"

[[DrWatson]]
deps = ["Dates", "FileIO", "LibGit2", "MacroTools", "Pkg", "Random", "Requires", "UnPack"]
git-tree-sha1 = "66bf318e41be55e721223464893566c81a782aa9"
uuid = "634d3b9d-ee7a-5ddf-bec9-22491ea816e1"
version = "2.3.0"

[[EarCut_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "92d8f9f208637e8d2d28c664051a00569c01493d"
uuid = "5ae413db-bbd1-5e63-b57d-d24a61df00f5"
version = "2.1.5+1"

[[EllipsisNotation]]
deps = ["ArrayInterface"]
git-tree-sha1 = "8041575f021cba5a099a456b4163c9a08b566a02"
uuid = "da5c29d0-fa7d-589e-88eb-ea29b0a81949"
version = "1.1.0"

[[Expat_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "b3bfd02e98aedfa5cf885665493c5598c350cd2f"
uuid = "2e619515-83b5-522b-bb60-26c02a35a201"
version = "2.2.10+0"

[[FFMPEG]]
deps = ["FFMPEG_jll"]
git-tree-sha1 = "b57e3acbe22f8484b4b5ff66a7499717fe1a9cc8"
uuid = "c87230d0-a227-11e9-1b43-d7ebe4e7570a"
version = "0.4.1"

[[FFMPEG_jll]]
deps = ["Artifacts", "Bzip2_jll", "FreeType2_jll", "FriBidi_jll", "JLLWrappers", "LAME_jll", "Libdl", "Ogg_jll", "OpenSSL_jll", "Opus_jll", "Pkg", "Zlib_jll", "libass_jll", "libfdk_aac_jll", "libvorbis_jll", "x264_jll", "x265_jll"]
git-tree-sha1 = "d8a578692e3077ac998b50c0217dfd67f21d1e5f"
uuid = "b22a6f82-2f65-5046-a5b2-351ab43fb4e5"
version = "4.4.0+0"

[[FFTW]]
deps = ["AbstractFFTs", "FFTW_jll", "LinearAlgebra", "MKL_jll", "Preferences", "Reexport"]
git-tree-sha1 = "f985af3b9f4e278b1d24434cbb546d6092fca661"
uuid = "7a1cc6ca-52ef-59f5-83cd-3a7055c09341"
version = "1.4.3"

[[FFTW_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "3676abafff7e4ff07bbd2c42b3d8201f31653dcc"
uuid = "f5851436-0d7a-5f13-b9de-f02708fd171a"
version = "3.3.9+8"

[[FileIO]]
deps = ["Pkg", "Requires", "UUIDs"]
git-tree-sha1 = "937c29268e405b6808d958a9ac41bfe1a31b08e7"
uuid = "5789e2e9-d7fb-5bc7-8068-2c6fae9b9549"
version = "1.11.0"

[[FillArrays]]
deps = ["LinearAlgebra", "Random", "SparseArrays", "Statistics"]
git-tree-sha1 = "7c365bdef6380b29cfc5caaf99688cd7489f9b87"
uuid = "1a297f60-69ca-5386-bcde-b61e274b549b"
version = "0.12.2"

[[FixedPointNumbers]]
deps = ["Statistics"]
git-tree-sha1 = "335bfdceacc84c5cdf16aadc768aa5ddfc5383cc"
uuid = "53c48c17-4a7d-5ca2-90c5-79b7896eea93"
version = "0.8.4"

[[Fontconfig_jll]]
deps = ["Artifacts", "Bzip2_jll", "Expat_jll", "FreeType2_jll", "JLLWrappers", "Libdl", "Libuuid_jll", "Pkg", "Zlib_jll"]
git-tree-sha1 = "21efd19106a55620a188615da6d3d06cd7f6ee03"
uuid = "a3f928ae-7b40-5064-980b-68af3947d34b"
version = "2.13.93+0"

[[Formatting]]
deps = ["Printf"]
git-tree-sha1 = "8339d61043228fdd3eb658d86c926cb282ae72a8"
uuid = "59287772-0a20-5a39-b81b-1366585eb4c0"
version = "0.4.2"

[[FreeType]]
deps = ["CEnum", "FreeType2_jll"]
git-tree-sha1 = "cabd77ab6a6fdff49bfd24af2ebe76e6e018a2b4"
uuid = "b38be410-82b0-50bf-ab77-7b57e271db43"
version = "4.0.0"

[[FreeType2_jll]]
deps = ["Artifacts", "Bzip2_jll", "JLLWrappers", "Libdl", "Pkg", "Zlib_jll"]
git-tree-sha1 = "87eb71354d8ec1a96d4a7636bd57a7347dde3ef9"
uuid = "d7e528f0-a631-5988-bf34-fe36492bcfd7"
version = "2.10.4+0"

[[FreeTypeAbstraction]]
deps = ["ColorVectorSpace", "Colors", "FreeType", "GeometryBasics", "StaticArrays"]
git-tree-sha1 = "19d0f1e234c13bbfd75258e55c52aa1d876115f5"
uuid = "663a7486-cb36-511b-a19d-713bb74d65c9"
version = "0.9.2"

[[FriBidi_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "aa31987c2ba8704e23c6c8ba8a4f769d5d7e4f91"
uuid = "559328eb-81f9-559d-9380-de523a88c83c"
version = "1.0.10+0"

[[Future]]
deps = ["Random"]
uuid = "9fa8497b-333b-5362-9e8d-4d0656e87820"

[[GLFW]]
deps = ["GLFW_jll"]
git-tree-sha1 = "35dbc482f0967d8dceaa7ce007d16f9064072166"
uuid = "f7f18e0c-5ee9-5ccd-a5bf-e8befd85ed98"
version = "3.4.1"

[[GLFW_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Libglvnd_jll", "Pkg", "Xorg_libXcursor_jll", "Xorg_libXi_jll", "Xorg_libXinerama_jll", "Xorg_libXrandr_jll"]
git-tree-sha1 = "dba1e8614e98949abfa60480b13653813d8f0157"
uuid = "0656b61e-2033-5cc2-a64a-77c0f6c09b89"
version = "3.3.5+0"

[[GLMakie]]
deps = ["ColorTypes", "Colors", "FileIO", "FixedPointNumbers", "FreeTypeAbstraction", "GLFW", "GeometryBasics", "LinearAlgebra", "Makie", "Markdown", "MeshIO", "ModernGL", "Observables", "Printf", "Serialization", "ShaderAbstractions", "StaticArrays"]
git-tree-sha1 = "c1d85695246bbb91eb7b952be04e23a5732e8fbc"
uuid = "e9467ef8-e4e7-5192-8a1a-b1aee30e663a"
version = "0.4.6"

[[GeometryBasics]]
deps = ["EarCut_jll", "IterTools", "LinearAlgebra", "StaticArrays", "StructArrays", "Tables"]
git-tree-sha1 = "58bcdf5ebc057b085e58d95c138725628dd7453c"
uuid = "5c1252a2-5f33-56bf-86c9-59e7332b4326"
version = "0.4.1"

[[GeometryTypes]]
deps = ["ColorTypes", "FixedPointNumbers", "LinearAlgebra", "StaticArrays"]
git-tree-sha1 = "07194161fe4e181c6bf51ef2e329ec4e7d050fc4"
uuid = "4d00f742-c7ba-57c2-abde-4428a4b178cb"
version = "0.8.4"

[[Gettext_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "JLLWrappers", "Libdl", "Libiconv_jll", "Pkg", "XML2_jll"]
git-tree-sha1 = "9b02998aba7bf074d14de89f9d37ca24a1a0b046"
uuid = "78b55507-aeef-58d4-861c-77aaff3498b1"
version = "0.21.0+0"

[[Glib_jll]]
deps = ["Artifacts", "Gettext_jll", "JLLWrappers", "Libdl", "Libffi_jll", "Libiconv_jll", "Libmount_jll", "PCRE_jll", "Pkg", "Zlib_jll"]
git-tree-sha1 = "7bf67e9a481712b3dbe9cb3dac852dc4b1162e02"
uuid = "7746bdde-850d-59dc-9ae8-88ece973131d"
version = "2.68.3+0"

[[GraphRecipes]]
deps = ["AbstractTrees", "GeometryTypes", "InteractiveUtils", "Interpolations", "LightGraphs", "LinearAlgebra", "NaNMath", "NetworkLayout", "PlotUtils", "RecipesBase", "SparseArrays", "Statistics"]
git-tree-sha1 = "79d9dc8ab02c4e753696b0d3b1096fabb7d74da3"
uuid = "bd48cda9-67a9-57be-86fa-5b3c104eda73"
version = "0.5.6"

[[Graphics]]
deps = ["Colors", "LinearAlgebra", "NaNMath"]
git-tree-sha1 = "2c1cf4df419938ece72de17f368a021ee162762e"
uuid = "a2bd30eb-e257-5431-a919-1863eab51364"
version = "1.1.0"

[[Graphite2_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "344bf40dcab1073aca04aa0df4fb092f920e4011"
uuid = "3b182d85-2403-5c21-9c21-1e1f0cc25472"
version = "1.3.14+0"

[[GridLayoutBase]]
deps = ["GeometryBasics", "InteractiveUtils", "Match", "Observables"]
git-tree-sha1 = "e2f606c87d09d5187bb6069dab8cee0af7c77bdb"
uuid = "3955a311-db13-416c-9275-1d80ed98e5e9"
version = "0.6.1"

[[Grisu]]
git-tree-sha1 = "53bb909d1151e57e2484c3d1b53e19552b887fb2"
uuid = "42e2da0e-8278-4e71-bc24-59509adca0fe"
version = "1.0.2"

[[HTTP]]
deps = ["Base64", "Dates", "IniFile", "MbedTLS", "Sockets"]
git-tree-sha1 = "c7ec02c4c6a039a98a15f955462cd7aea5df4508"
uuid = "cd3eb016-35fb-5094-929b-558a96fad6f3"
version = "0.8.19"

[[HarfBuzz_jll]]
deps = ["Artifacts", "Cairo_jll", "Fontconfig_jll", "FreeType2_jll", "Glib_jll", "Graphite2_jll", "JLLWrappers", "Libdl", "Libffi_jll", "Pkg"]
git-tree-sha1 = "8a954fed8ac097d5be04921d595f741115c1b2ad"
uuid = "2e76f6c2-a576-52d4-95c1-20adfe4de566"
version = "2.8.1+0"

[[Hyperscript]]
deps = ["Test"]
git-tree-sha1 = "8d511d5b81240fc8e6802386302675bdf47737b9"
uuid = "47d2ed2b-36de-50cf-bf87-49c2cf4b8b91"
version = "0.0.4"

[[IfElse]]
git-tree-sha1 = "28e837ff3e7a6c3cdb252ce49fb412c8eb3caeef"
uuid = "615f187c-cbe4-4ef1-ba3b-2fcf58d6d173"
version = "0.1.0"

[[ImageCore]]
deps = ["AbstractFFTs", "Colors", "FixedPointNumbers", "Graphics", "MappedArrays", "MosaicViews", "OffsetArrays", "PaddedViews", "Reexport"]
git-tree-sha1 = "db645f20b59f060d8cfae696bc9538d13fd86416"
uuid = "a09fc81d-aa75-5fe9-8630-4744c3626534"
version = "0.8.22"

[[ImageIO]]
deps = ["FileIO", "Netpbm", "OpenEXR", "PNGFiles", "TiffImages", "UUIDs"]
git-tree-sha1 = "13c826abd23931d909e4c5538643d9691f62a617"
uuid = "82e4d734-157c-48bb-816b-45c225c6df19"
version = "0.5.8"

[[Imath_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "87f7662e03a649cffa2e05bf19c303e168732d3e"
uuid = "905a6f67-0a94-5f89-b386-d35d92009cd1"
version = "3.1.2+0"

[[IndirectArrays]]
git-tree-sha1 = "c2a145a145dc03a7620af1444e0264ef907bd44f"
uuid = "9b13fd28-a010-5f03-acff-a1bbcff69959"
version = "0.5.1"

[[Inflate]]
git-tree-sha1 = "f5fc07d4e706b84f72d54eedcc1c13d92fb0871c"
uuid = "d25df0c9-e2be-5dd7-82c8-3ad0b3e990b9"
version = "0.1.2"

[[IniFile]]
deps = ["Test"]
git-tree-sha1 = "098e4d2c533924c921f9f9847274f2ad89e018b8"
uuid = "83e8ac13-25f8-5344-8a64-a9f2b223428f"
version = "0.5.0"

[[IntelOpenMP_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "d979e54b71da82f3a65b62553da4fc3d18c9004c"
uuid = "1d5cc7b8-4909-519e-a0f8-d0f5ad9712d0"
version = "2018.0.3+2"

[[InteractiveDynamics]]
deps = ["DataStructures", "Makie", "Observables", "Requires", "StatsBase", "Test"]
git-tree-sha1 = "90cc6eb66de8cb492e901acf4a0f2bdf1fc2f016"
uuid = "ec714cd0-5f51-11eb-0b6e-452e7367ff84"
version = "0.17.2"

[[InteractiveUtils]]
deps = ["Markdown"]
uuid = "b77e0a4c-d291-57a0-90e8-8db25a27a240"

[[Interpolations]]
deps = ["AxisAlgorithms", "ChainRulesCore", "LinearAlgebra", "OffsetArrays", "Random", "Ratios", "Requires", "SharedArrays", "SparseArrays", "StaticArrays", "WoodburyMatrices"]
git-tree-sha1 = "61aa005707ea2cebf47c8d780da8dc9bc4e0c512"
uuid = "a98d9a8b-a2ab-59e6-89dd-64a1c18fca59"
version = "0.13.4"

[[IntervalSets]]
deps = ["Dates", "EllipsisNotation", "Statistics"]
git-tree-sha1 = "3cc368af3f110a767ac786560045dceddfc16758"
uuid = "8197267c-284f-5f27-9208-e0e47529a953"
version = "0.5.3"

[[InvertedIndices]]
deps = ["Test"]
git-tree-sha1 = "15732c475062348b0165684ffe28e85ea8396afc"
uuid = "41ab1584-1d38-5bbf-9106-f11c6c58b48f"
version = "1.0.0"

[[IrrationalConstants]]
git-tree-sha1 = "f76424439413893a832026ca355fe273e93bce94"
uuid = "92d709cd-6900-40b7-9082-c6be49f344b6"
version = "0.1.0"

[[Isoband]]
deps = ["isoband_jll"]
git-tree-sha1 = "f9b6d97355599074dc867318950adaa6f9946137"
uuid = "f1662d9f-8043-43de-a69a-05efc1cc6ff4"
version = "0.1.1"

[[IterTools]]
git-tree-sha1 = "05110a2ab1fc5f932622ffea2a003221f4782c18"
uuid = "c8e1da08-722c-5040-9ed9-7db0dc04731e"
version = "1.3.0"

[[IteratorInterfaceExtensions]]
git-tree-sha1 = "a3f24677c21f5bbe9d2a714f95dcd58337fb2856"
uuid = "82899510-4779-5014-852e-03e436cf321d"
version = "1.0.0"

[[JLD2]]
deps = ["DataStructures", "FileIO", "MacroTools", "Mmap", "Pkg", "Printf", "Reexport", "TranscodingStreams", "UUIDs"]
git-tree-sha1 = "59ee430ac5dc87bc3eec833cc2a37853425750b4"
uuid = "033835bb-8acc-5ee8-8aae-3f567f8a3819"
version = "0.4.13"

[[JLLWrappers]]
deps = ["Preferences"]
git-tree-sha1 = "642a199af8b68253517b80bd3bfd17eb4e84df6e"
uuid = "692b3bcd-3c85-4b1f-b108-f13ce0eb3210"
version = "1.3.0"

[[JSON]]
deps = ["Dates", "Mmap", "Parsers", "Unicode"]
git-tree-sha1 = "8076680b162ada2a031f707ac7b4953e30667a37"
uuid = "682c06a0-de6a-54ab-a142-c8b1cf79cde6"
version = "0.21.2"

[[JSON3]]
deps = ["Dates", "Mmap", "Parsers", "StructTypes", "UUIDs"]
git-tree-sha1 = "b3e5984da3c6c95bcf6931760387ff2e64f508f3"
uuid = "0f8b85d8-7281-11e9-16c2-39a750bddbf1"
version = "1.9.1"

[[JSServe]]
deps = ["Base64", "CodecZlib", "Colors", "HTTP", "Hyperscript", "JSON3", "LinearAlgebra", "Markdown", "MsgPack", "Observables", "SHA", "Sockets", "Tables", "Test", "UUIDs", "WebSockets", "WidgetsBase"]
git-tree-sha1 = "91101a4b8ac8eefeed6ca8eb4f663fc660e4d9f9"
uuid = "824d6782-a2ef-11e9-3a09-e5662e0c26f9"
version = "1.2.3"

[[KernelDensity]]
deps = ["Distributions", "DocStringExtensions", "FFTW", "Interpolations", "StatsBase"]
git-tree-sha1 = "591e8dc09ad18386189610acafb970032c519707"
uuid = "5ab0869b-81aa-558d-bb23-cbf5423bbe9b"
version = "0.6.3"

[[LAME_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "f6250b16881adf048549549fba48b1161acdac8c"
uuid = "c1c5ebd0-6772-5130-a774-d5fcae4a789d"
version = "3.100.1+0"

[[LZO_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "e5b909bcf985c5e2605737d2ce278ed791b89be6"
uuid = "dd4b983a-f0e5-5f8d-a1b7-129d4a5fb1ac"
version = "2.10.1+0"

[[LaTeXStrings]]
git-tree-sha1 = "c7f1c695e06c01b95a67f0cd1d34994f3e7db104"
uuid = "b964fa9f-0449-5b57-a5c2-d3ea65f4040f"
version = "1.2.1"

[[LazyArtifacts]]
deps = ["Artifacts", "Pkg"]
uuid = "4af54fe1-eca0-43a8-85a7-787d91b784e3"

[[LibCURL]]
deps = ["LibCURL_jll", "MozillaCACerts_jll"]
uuid = "b27032c2-a3e7-50c8-80cd-2d36dbcbfd21"

[[LibCURL_jll]]
deps = ["Artifacts", "LibSSH2_jll", "Libdl", "MbedTLS_jll", "Zlib_jll", "nghttp2_jll"]
uuid = "deac9b47-8bc7-5906-a0fe-35ac56dc84c0"

[[LibExpat]]
deps = ["Expat_jll", "Pkg"]
git-tree-sha1 = "27dc51f94ceb107fd53b367431a638b430e01e81"
uuid = "522f3ed2-3f36-55e3-b6df-e94fee9b0c07"
version = "0.6.1"

[[LibGit2]]
deps = ["Base64", "NetworkOptions", "Printf", "SHA"]
uuid = "76f85450-5226-5b5a-8eaa-529ad045b433"

[[LibSSH2_jll]]
deps = ["Artifacts", "Libdl", "MbedTLS_jll"]
uuid = "29816b5a-b9ab-546f-933c-edad1886dfa8"

[[Libdl]]
uuid = "8f399da3-3557-5675-b5ff-fb832c97cbdb"

[[Libffi_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "761a393aeccd6aa92ec3515e428c26bf99575b3b"
uuid = "e9f186c6-92d2-5b65-8a66-fee21dc1b490"
version = "3.2.2+0"

[[Libgcrypt_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Libgpg_error_jll", "Pkg"]
git-tree-sha1 = "64613c82a59c120435c067c2b809fc61cf5166ae"
uuid = "d4300ac3-e22c-5743-9152-c294e39db1e4"
version = "1.8.7+0"

[[Libglvnd_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libX11_jll", "Xorg_libXext_jll"]
git-tree-sha1 = "7739f837d6447403596a75d19ed01fd08d6f56bf"
uuid = "7e76a0d4-f3c7-5321-8279-8d96eeed0f29"
version = "1.3.0+3"

[[Libgpg_error_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "c333716e46366857753e273ce6a69ee0945a6db9"
uuid = "7add5ba3-2f88-524e-9cd5-f83b8a55f7b8"
version = "1.42.0+0"

[[Libiconv_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "42b62845d70a619f063a7da093d995ec8e15e778"
uuid = "94ce4f54-9a6c-5748-9c1c-f9c7231a4531"
version = "1.16.1+1"

[[Libmount_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "9c30530bf0effd46e15e0fdcf2b8636e78cbbd73"
uuid = "4b2f31a3-9ecc-558c-b454-b3730dcb73e9"
version = "2.35.0+0"

[[Libuuid_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "7f3efec06033682db852f8b3bc3c1d2b0a0ab066"
uuid = "38a345b3-de98-5d2b-a5d3-14cd9215e700"
version = "2.36.0+0"

[[LightGraphs]]
deps = ["ArnoldiMethod", "DataStructures", "Distributed", "Inflate", "LinearAlgebra", "Random", "SharedArrays", "SimpleTraits", "SparseArrays", "Statistics"]
git-tree-sha1 = "432428df5f360964040ed60418dd5601ecd240b6"
uuid = "093fc24a-ae57-5d10-9952-331d41423f4d"
version = "1.3.5"

[[LinearAlgebra]]
deps = ["Libdl"]
uuid = "37e2e46d-f89d-539d-b4ee-838fcccc9c8e"

[[LogExpFunctions]]
deps = ["DocStringExtensions", "IrrationalConstants", "LinearAlgebra"]
git-tree-sha1 = "3d682c07e6dd250ed082f883dc88aee7996bf2cc"
uuid = "2ab3a3ac-af41-5b50-aa03-7779005ae688"
version = "0.3.0"

[[Logging]]
uuid = "56ddb016-857b-54e1-b83d-db4d58db5568"

[[MKL_jll]]
deps = ["Artifacts", "IntelOpenMP_jll", "JLLWrappers", "LazyArtifacts", "Libdl", "Pkg"]
git-tree-sha1 = "c253236b0ed414624b083e6b72bfe891fbd2c7af"
uuid = "856f044c-d86e-5d09-b602-aeab76dc8ba7"
version = "2021.1.1+1"

[[MacroTools]]
deps = ["Markdown", "Random"]
git-tree-sha1 = "0fb723cd8c45858c22169b2e42269e53271a6df7"
uuid = "1914dd2f-81c6-5fcd-8719-6d5c9610ff09"
version = "0.5.7"

[[Makie]]
deps = ["Animations", "Base64", "ColorBrewer", "ColorSchemes", "ColorTypes", "Colors", "Contour", "Distributions", "DocStringExtensions", "FFMPEG", "FileIO", "FixedPointNumbers", "Formatting", "FreeType", "FreeTypeAbstraction", "GeometryBasics", "GridLayoutBase", "ImageIO", "IntervalSets", "Isoband", "KernelDensity", "LaTeXStrings", "LinearAlgebra", "MakieCore", "Markdown", "Match", "MathTeXEngine", "Observables", "Packing", "PlotUtils", "PolygonOps", "Printf", "Random", "RelocatableFolders", "Serialization", "Showoff", "SignedDistanceFields", "SparseArrays", "StaticArrays", "Statistics", "StatsBase", "StatsFuns", "StructArrays", "UnicodeFun"]
git-tree-sha1 = "7e49f989e7c7f50fe55bd92d45329c9cf3f2583d"
uuid = "ee78f7c6-11fb-53f2-987a-cfe4a2b5a57a"
version = "0.15.2"

[[MakieCore]]
deps = ["Observables"]
git-tree-sha1 = "7bcc8323fb37523a6a51ade2234eee27a11114c8"
uuid = "20f20a25-4f0e-4fdf-b5d1-57303727442b"
version = "0.1.3"

[[MappedArrays]]
git-tree-sha1 = "e8b359ef06ec72e8c030463fe02efe5527ee5142"
uuid = "dbb5928d-eab1-5f90-85c2-b9b0edb7c900"
version = "0.4.1"

[[Markdown]]
deps = ["Base64"]
uuid = "d6f4376e-aef5-505a-96c1-9c027394607a"

[[Match]]
git-tree-sha1 = "5cf525d97caf86d29307150fcba763a64eaa9cbe"
uuid = "7eb4fadd-790c-5f42-8a69-bfa0b872bfbf"
version = "1.1.0"

[[MathTeXEngine]]
deps = ["AbstractTrees", "Automa", "DataStructures", "FreeTypeAbstraction", "GeometryBasics", "LaTeXStrings", "REPL", "RelocatableFolders", "Test"]
git-tree-sha1 = "f5c8789464aed7058107463e5cef53e6ad3f1f3e"
uuid = "0a4f8689-d25c-4efe-a92b-7142dfc1aa53"
version = "0.2.0"

[[MbedTLS]]
deps = ["Dates", "MbedTLS_jll", "Random", "Sockets"]
git-tree-sha1 = "1c38e51c3d08ef2278062ebceade0e46cefc96fe"
uuid = "739be429-bea8-5141-9913-cc70e7f3736d"
version = "1.0.3"

[[MbedTLS_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "c8ffd9c3-330d-5841-b78e-0817d7145fa1"

[[MeshIO]]
deps = ["ColorTypes", "FileIO", "GeometryBasics", "Printf"]
git-tree-sha1 = "38f4e93a6485dbd610f09a0669741d2f4899e9ec"
uuid = "7269a6da-0436-5bbc-96c2-40638cbb6118"
version = "0.4.9"

[[Missings]]
deps = ["DataAPI"]
git-tree-sha1 = "2ca267b08821e86c5ef4376cffed98a46c2cb205"
uuid = "e1d29d7a-bbdc-5cf2-9ac0-f12de2c33e28"
version = "1.0.1"

[[Mmap]]
uuid = "a63ad114-7e13-5084-954f-fe012c677804"

[[ModernGL]]
deps = ["Libdl"]
git-tree-sha1 = "326957556e9cc9253615114c04bb0096a2a69bb8"
uuid = "66fc600b-dfda-50eb-8b99-91cfa97b1301"
version = "1.1.2"

[[MosaicViews]]
deps = ["MappedArrays", "OffsetArrays", "PaddedViews", "StackViews"]
git-tree-sha1 = "b34e3bc3ca7c94914418637cb10cc4d1d80d877d"
uuid = "e94cdb99-869f-56ef-bcf0-1ae2bcbe0389"
version = "0.3.3"

[[MozillaCACerts_jll]]
uuid = "14a3606d-f60d-562e-9121-12d972cd8159"

[[MsgPack]]
deps = ["Serialization"]
git-tree-sha1 = "a8cbf066b54d793b9a48c5daa5d586cf2b5bd43d"
uuid = "99f44e22-a591-53d1-9472-aa23ef4bd671"
version = "1.1.0"

[[NaNMath]]
git-tree-sha1 = "bfe47e760d60b82b66b61d2d44128b62e3a369fb"
uuid = "77ba4419-2d1f-58cd-9bb1-8ffee604a2e3"
version = "0.3.5"

[[Netpbm]]
deps = ["ColorVectorSpace", "FileIO", "ImageCore"]
git-tree-sha1 = "09589171688f0039f13ebe0fdcc7288f50228b52"
uuid = "f09324ee-3d7c-5217-9330-fc30815ba969"
version = "1.0.1"

[[NetworkLayout]]
deps = ["DelimitedFiles", "GeometryTypes", "LinearAlgebra", "SparseArrays", "Test"]
git-tree-sha1 = "adde9ad01842a4f62c3724b8707bed90293f0f3f"
uuid = "46757867-2c16-5918-afeb-47bfcb05e46a"
version = "0.2.0"

[[NetworkOptions]]
uuid = "ca575930-c2e3-43a9-ace4-1e988b2c1908"

[[Observables]]
git-tree-sha1 = "fe29afdef3d0c4a8286128d4e45cc50621b1e43d"
uuid = "510215fc-4207-5dde-b226-833fc4488ee2"
version = "0.4.0"

[[OffsetArrays]]
deps = ["Adapt"]
git-tree-sha1 = "c870a0d713b51e4b49be6432eff0e26a4325afee"
uuid = "6fe1bfb0-de20-5000-8ca7-80f57d26f881"
version = "1.10.6"

[[Ogg_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "7937eda4681660b4d6aeeecc2f7e1c81c8ee4e2f"
uuid = "e7412a2a-1a6e-54c0-be00-318e2571c051"
version = "1.3.5+0"

[[OpenEXR]]
deps = ["Colors", "FileIO", "OpenEXR_jll"]
git-tree-sha1 = "327f53360fdb54df7ecd01e96ef1983536d1e633"
uuid = "52e1d378-f018-4a11-a4be-720524705ac7"
version = "0.3.2"

[[OpenEXR_jll]]
deps = ["Artifacts", "Imath_jll", "JLLWrappers", "Libdl", "Pkg", "Zlib_jll"]
git-tree-sha1 = "923319661e9a22712f24596ce81c54fc0366f304"
uuid = "18a262bb-aa17-5467-a713-aee519bc75cb"
version = "3.1.1+0"

[[OpenSSL_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "15003dcb7d8db3c6c857fda14891a539a8f2705a"
uuid = "458c3c95-2e84-50aa-8efc-19380b2a3a95"
version = "1.1.10+0"

[[OpenSpecFun_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "13652491f6856acfd2db29360e1bbcd4565d04f1"
uuid = "efe28fd5-8261-553b-a9e1-b2916fc3738e"
version = "0.5.5+0"

[[OpenStreetMapX]]
deps = ["CodecZlib", "DataStructures", "HTTP", "JSON", "LibExpat", "LightGraphs", "ProtoBuf", "Random", "Serialization", "SparseArrays", "StableRNGs", "Statistics"]
git-tree-sha1 = "7693807742ea6e8d7b5dd94fa0dfb007b52e0821"
uuid = "86cd37e6-c0ff-550b-95fe-21d72c8d4fc9"
version = "0.2.4"

[[Opus_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "51a08fb14ec28da2ec7a927c4337e4332c2a4720"
uuid = "91d4177d-7536-5919-b921-800302f37372"
version = "1.3.2+0"

[[OrderedCollections]]
git-tree-sha1 = "85f8e6578bf1f9ee0d11e7bb1b1456435479d47c"
uuid = "bac558e1-5e72-5ebc-8fee-abe8a469f55d"
version = "1.4.1"

[[PCRE_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "b2a7af664e098055a7529ad1a900ded962bca488"
uuid = "2f80f16e-611a-54ab-bc61-aa92de5b98fc"
version = "8.44.0+0"

[[PDMats]]
deps = ["LinearAlgebra", "SparseArrays", "SuiteSparse"]
git-tree-sha1 = "4dd403333bcf0909341cfe57ec115152f937d7d8"
uuid = "90014a1f-27ba-587c-ab20-58faa44d9150"
version = "0.11.1"

[[PNGFiles]]
deps = ["Base64", "CEnum", "ImageCore", "IndirectArrays", "OffsetArrays", "libpng_jll"]
git-tree-sha1 = "520e28d4026d16dcf7b8c8140a3041f0e20a9ca8"
uuid = "f57f5aa1-a3ce-4bc8-8ab9-96f992907883"
version = "0.3.7"

[[Packing]]
deps = ["GeometryBasics"]
git-tree-sha1 = "1155f6f937fa2b94104162f01fa400e192e4272f"
uuid = "19eb6ba3-879d-56ad-ad62-d5c202156566"
version = "0.4.2"

[[PaddedViews]]
deps = ["OffsetArrays"]
git-tree-sha1 = "646eed6f6a5d8df6708f15ea7e02a7a2c4fe4800"
uuid = "5432bcbf-9aad-5242-b902-cca2824c8663"
version = "0.5.10"

[[Parsers]]
deps = ["Dates"]
git-tree-sha1 = "bfd7d8c7fd87f04543810d9cbd3995972236ba1b"
uuid = "69de0a69-1ddd-5017-9359-2bf0b02dc9f0"
version = "1.1.2"

[[Pixman_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "b4f5d02549a10e20780a24fce72bea96b6329e29"
uuid = "30392449-352a-5448-841d-b1acce4e97dc"
version = "0.40.1+0"

[[Pkg]]
deps = ["Artifacts", "Dates", "Downloads", "LibGit2", "Libdl", "Logging", "Markdown", "Printf", "REPL", "Random", "SHA", "Serialization", "TOML", "Tar", "UUIDs", "p7zip_jll"]
uuid = "44cfe95a-1eb2-52ea-b672-e2afdf69b78f"

[[PkgVersion]]
deps = ["Pkg"]
git-tree-sha1 = "a7a7e1a88853564e551e4eba8650f8c38df79b37"
uuid = "eebad327-c553-4316-9ea0-9fa01ccd7688"
version = "0.1.1"

[[PlotUtils]]
deps = ["ColorSchemes", "Colors", "Dates", "Printf", "Random", "Reexport", "Statistics"]
git-tree-sha1 = "9ff1c70190c1c30aebca35dc489f7411b256cd23"
uuid = "995b91a9-d308-5afd-9ec6-746e21dbc043"
version = "1.0.13"

[[PolygonOps]]
git-tree-sha1 = "c031d2332c9a8e1c90eca239385815dc271abb22"
uuid = "647866c9-e3ac-4575-94e7-e3d426903924"
version = "0.1.1"

[[PooledArrays]]
deps = ["DataAPI", "Future"]
git-tree-sha1 = "cde4ce9d6f33219465b55162811d8de8139c0414"
uuid = "2dfb63ee-cc39-5dd5-95bd-886bf059d720"
version = "1.2.1"

[[Preferences]]
deps = ["TOML"]
git-tree-sha1 = "00cfd92944ca9c760982747e9a1d0d5d86ab1e5a"
uuid = "21216c6a-2e73-6563-6e65-726566657250"
version = "1.2.2"

[[PrettyTables]]
deps = ["Crayons", "Formatting", "Markdown", "Reexport", "Tables"]
git-tree-sha1 = "0d1245a357cc61c8cd61934c07447aa569ff22e6"
uuid = "08abe8d2-0d0c-5749-adfa-8a2ac140af0d"
version = "1.1.0"

[[Printf]]
deps = ["Unicode"]
uuid = "de0858da-6303-5e67-8744-51eddeeeb8d7"

[[ProgressMeter]]
deps = ["Distributed", "Printf"]
git-tree-sha1 = "afadeba63d90ff223a6a48d2009434ecee2ec9e8"
uuid = "92933f4c-e287-5a05-a399-4b506db050ca"
version = "1.7.1"

[[ProtoBuf]]
deps = ["Logging", "protoc_jll"]
git-tree-sha1 = "3142e13697a7f6363798d1241daf3e6cead83008"
uuid = "3349acd9-ac6a-5e09-bcdb-63829b23a429"
version = "0.11.2"

[[QuadGK]]
deps = ["DataStructures", "LinearAlgebra"]
git-tree-sha1 = "12fbe86da16df6679be7521dfb39fbc861e1dc7b"
uuid = "1fd47b50-473d-5c70-9696-f719f8f3bcdc"
version = "2.4.1"

[[REPL]]
deps = ["InteractiveUtils", "Markdown", "Sockets", "Unicode"]
uuid = "3fa0cd96-eef1-5676-8a61-b3b8758bbffb"

[[Random]]
deps = ["Serialization"]
uuid = "9a3f8284-a2c9-5f02-9a11-845980a1fd5c"

[[Ratios]]
deps = ["Requires"]
git-tree-sha1 = "7dff99fbc740e2f8228c6878e2aad6d7c2678098"
uuid = "c84ed2f1-dad5-54f0-aa8e-dbefe2724439"
version = "0.4.1"

[[RecipesBase]]
git-tree-sha1 = "44a75aa7a527910ee3d1751d1f0e4148698add9e"
uuid = "3cdcf5f2-1ef4-517c-9805-6587b60abb01"
version = "1.1.2"

[[Reexport]]
git-tree-sha1 = "45e428421666073eab6f2da5c9d310d99bb12f9b"
uuid = "189a3867-3050-52da-a836-e630ba90ab69"
version = "1.2.2"

[[RelocatableFolders]]
deps = ["SHA", "Scratch"]
git-tree-sha1 = "0529f4188bc8efee85a7e580aca1c7dff6b103f8"
uuid = "05181044-ff0b-4ac5-8273-598c1e38db00"
version = "0.1.0"

[[Requires]]
deps = ["UUIDs"]
git-tree-sha1 = "4036a3bd08ac7e968e27c203d45f5fff15020621"
uuid = "ae029012-a4dd-5104-9daa-d747884805df"
version = "1.1.3"

[[Rmath]]
deps = ["Random", "Rmath_jll"]
git-tree-sha1 = "bf3188feca147ce108c76ad82c2792c57abe7b1f"
uuid = "79098fc4-a85e-5d69-aa6a-4863f24498fa"
version = "0.7.0"

[[Rmath_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "68db32dff12bb6127bac73c209881191bf0efbb7"
uuid = "f50d1b31-88e8-58de-be2c-1cc44531875f"
version = "0.3.0+0"

[[SHA]]
uuid = "ea8e919c-243c-51af-8825-aaa63cd721ce"

[[SIMD]]
git-tree-sha1 = "9ba33637b24341aba594a2783a502760aa0bff04"
uuid = "fdea26ae-647d-5447-a871-4b548cad5224"
version = "3.3.1"

[[ScanByte]]
deps = ["Libdl", "SIMD"]
git-tree-sha1 = "9cc2955f2a254b18be655a4ee70bc4031b2b189e"
uuid = "7b38b023-a4d7-4c5e-8d43-3f3097f304eb"
version = "0.3.0"

[[Scratch]]
deps = ["Dates"]
git-tree-sha1 = "0b4b7f1393cff97c33891da2a0bf69c6ed241fda"
uuid = "6c6a2e73-6563-6170-7368-637461726353"
version = "1.1.0"

[[SentinelArrays]]
deps = ["Dates", "Random"]
git-tree-sha1 = "54f37736d8934a12a200edea2f9206b03bdf3159"
uuid = "91c51154-3ec4-41a3-a24f-3f23e20d615c"
version = "1.3.7"

[[Serialization]]
uuid = "9e88b42a-f829-5b0c-bbe9-9e923198166b"

[[ShaderAbstractions]]
deps = ["ColorTypes", "FixedPointNumbers", "GeometryBasics", "LinearAlgebra", "Observables", "StaticArrays", "StructArrays", "Tables"]
git-tree-sha1 = "0d97c895406b552bed78f3a1fe9925248e908ae2"
uuid = "65257c39-d410-5151-9873-9b3e5be5013e"
version = "0.2.8"

[[SharedArrays]]
deps = ["Distributed", "Mmap", "Random", "Serialization"]
uuid = "1a1011a3-84de-559e-8e89-a11a2f7dc383"

[[Showoff]]
deps = ["Dates", "Grisu"]
git-tree-sha1 = "91eddf657aca81df9ae6ceb20b959ae5653ad1de"
uuid = "992d4aef-0814-514b-bc4d-f2e9a6c4116f"
version = "1.0.3"

[[SignedDistanceFields]]
deps = ["Random", "Statistics", "Test"]
git-tree-sha1 = "d263a08ec505853a5ff1c1ebde2070419e3f28e9"
uuid = "73760f76-fbc4-59ce-8f25-708e95d2df96"
version = "0.4.0"

[[SimpleTraits]]
deps = ["InteractiveUtils", "MacroTools"]
git-tree-sha1 = "5d7e3f4e11935503d3ecaf7186eac40602e7d231"
uuid = "699a6c99-e7fa-54fc-8d76-47d257e15c1d"
version = "0.9.4"

[[Sockets]]
uuid = "6462fe0b-24de-5631-8697-dd941f90decc"

[[SortingAlgorithms]]
deps = ["DataStructures"]
git-tree-sha1 = "b3363d7460f7d098ca0912c69b082f75625d7508"
uuid = "a2af1166-a08f-5f64-846c-94a0d3cef48c"
version = "1.0.1"

[[SparseArrays]]
deps = ["LinearAlgebra", "Random"]
uuid = "2f01184e-e22b-5df5-ae63-d93ebab69eaf"

[[SpecialFunctions]]
deps = ["ChainRulesCore", "LogExpFunctions", "OpenSpecFun_jll"]
git-tree-sha1 = "a322a9493e49c5f3a10b50df3aedaf1cdb3244b7"
uuid = "276daf66-3868-5448-9aa4-cd146d93841b"
version = "1.6.1"

[[StableRNGs]]
deps = ["Random", "Test"]
git-tree-sha1 = "3be7d49667040add7ee151fefaf1f8c04c8c8276"
uuid = "860ef19b-820b-49d6-a774-d7a799459cd3"
version = "1.0.0"

[[StackViews]]
deps = ["OffsetArrays"]
git-tree-sha1 = "46e589465204cd0c08b4bd97385e4fa79a0c770c"
uuid = "cae243ae-269e-4f55-b966-ac2d0dc13c15"
version = "0.1.1"

[[Static]]
deps = ["IfElse"]
git-tree-sha1 = "854b024a4a81b05c0792a4b45293b85db228bd27"
uuid = "aedffcd0-7271-4cad-89d0-dc628f76c6d3"
version = "0.3.1"

[[StaticArrays]]
deps = ["LinearAlgebra", "Random", "Statistics"]
git-tree-sha1 = "3240808c6d463ac46f1c1cd7638375cd22abbccb"
uuid = "90137ffa-7385-5640-81b9-e52037218182"
version = "1.2.12"

[[Statistics]]
deps = ["LinearAlgebra", "SparseArrays"]
uuid = "10745b16-79ce-11e8-11f9-7d13ad32a3b2"

[[StatsAPI]]
git-tree-sha1 = "1958272568dc176a1d881acb797beb909c785510"
uuid = "82ae8749-77ed-4fe6-ae5f-f523153014b0"
version = "1.0.0"

[[StatsBase]]
deps = ["DataAPI", "DataStructures", "LinearAlgebra", "Missings", "Printf", "Random", "SortingAlgorithms", "SparseArrays", "Statistics", "StatsAPI"]
git-tree-sha1 = "8cbbc098554648c84f79a463c9ff0fd277144b6c"
uuid = "2913bbd2-ae8a-5f71-8c99-4fb6c76f3a91"
version = "0.33.10"

[[StatsFuns]]
deps = ["ChainRulesCore", "IrrationalConstants", "LogExpFunctions", "Reexport", "Rmath", "SpecialFunctions"]
git-tree-sha1 = "46d7ccc7104860c38b11966dd1f72ff042f382e4"
uuid = "4c63d2b9-4356-54db-8cca-17b64c39e42c"
version = "0.9.10"

[[StructArrays]]
deps = ["Adapt", "DataAPI", "StaticArrays", "Tables"]
git-tree-sha1 = "1700b86ad59348c0f9f68ddc95117071f947072d"
uuid = "09ab397b-f2b6-538f-b94a-2f83cf4a842a"
version = "0.6.1"

[[StructTypes]]
deps = ["Dates", "UUIDs"]
git-tree-sha1 = "8445bf99a36d703a09c601f9a57e2f83000ef2ae"
uuid = "856f2bd8-1eba-4b0a-8007-ebc267875bd4"
version = "1.7.3"

[[SuiteSparse]]
deps = ["Libdl", "LinearAlgebra", "Serialization", "SparseArrays"]
uuid = "4607b0f0-06f3-5cda-b6b1-a6196a1729e9"

[[TOML]]
deps = ["Dates"]
uuid = "fa267f1f-6049-4f14-aa54-33bafae1ed76"

[[TableTraits]]
deps = ["IteratorInterfaceExtensions"]
git-tree-sha1 = "c06b2f539df1c6efa794486abfb6ed2022561a39"
uuid = "3783bdb8-4a98-5b6b-af9a-565f29a5fe9c"
version = "1.0.1"

[[Tables]]
deps = ["DataAPI", "DataValueInterfaces", "IteratorInterfaceExtensions", "LinearAlgebra", "TableTraits", "Test"]
git-tree-sha1 = "d0c690d37c73aeb5ca063056283fde5585a41710"
uuid = "bd369af6-aec1-5ad0-b16a-f7cc5008161c"
version = "1.5.0"

[[Tar]]
deps = ["ArgTools", "SHA"]
uuid = "a4e569a6-e804-4fa4-b0f3-eef7a1d5b13e"

[[Test]]
deps = ["InteractiveUtils", "Logging", "Random", "Serialization"]
uuid = "8dfed614-e22c-5e08-85e1-65c5234f0b40"

[[TiffImages]]
deps = ["ColorTypes", "DocStringExtensions", "FileIO", "FixedPointNumbers", "IndirectArrays", "Inflate", "OffsetArrays", "OrderedCollections", "PkgVersion", "ProgressMeter"]
git-tree-sha1 = "03fb246ac6e6b7cb7abac3b3302447d55b43270e"
uuid = "731e570b-9d59-4bfa-96dc-6df516fadf69"
version = "0.4.1"

[[TranscodingStreams]]
deps = ["Random", "Test"]
git-tree-sha1 = "216b95ea110b5972db65aa90f88d8d89dcb8851c"
uuid = "3bb67fe8-82b1-5028-8e26-92a6c54297fa"
version = "0.9.6"

[[UUIDs]]
deps = ["Random", "SHA"]
uuid = "cf7118a7-6976-5b1a-9a39-7adc72f591a4"

[[UnPack]]
git-tree-sha1 = "387c1f73762231e86e0c9c5443ce3b4a0a9a0c2b"
uuid = "3a884ed6-31ef-47d7-9d2a-63182c4928ed"
version = "1.0.2"

[[Unicode]]
uuid = "4ec0a83e-493e-50e2-b9ac-8f72acf5a8f5"

[[UnicodeFun]]
deps = ["REPL"]
git-tree-sha1 = "53915e50200959667e78a92a418594b428dffddf"
uuid = "1cfade01-22cf-5700-b092-accc4b62d6e1"
version = "0.4.1"

[[WebSockets]]
deps = ["Base64", "Dates", "HTTP", "Logging", "Sockets"]
git-tree-sha1 = "f91a602e25fe6b89afc93cf02a4ae18ee9384ce3"
uuid = "104b5d7c-a370-577a-8038-80a2059c5097"
version = "1.5.9"

[[WidgetsBase]]
deps = ["Observables"]
git-tree-sha1 = "c1ef6e02bc457c3b23aafc765b94c3dcd25f174d"
uuid = "eead4739-05f7-45a1-878c-cee36b57321c"
version = "0.1.3"

[[WoodburyMatrices]]
deps = ["LinearAlgebra", "SparseArrays"]
git-tree-sha1 = "59e2ad8fd1591ea019a5259bd012d7aee15f995c"
uuid = "efce3f68-66dc-5838-9240-27a6d6f5f9b6"
version = "0.5.3"

[[XML2_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Libiconv_jll", "Pkg", "Zlib_jll"]
git-tree-sha1 = "1acf5bdf07aa0907e0a37d3718bb88d4b687b74a"
uuid = "02c8fc9c-b97f-50b9-bbe4-9be30ff0a78a"
version = "2.9.12+0"

[[XSLT_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Libgcrypt_jll", "Libgpg_error_jll", "Libiconv_jll", "Pkg", "XML2_jll", "Zlib_jll"]
git-tree-sha1 = "91844873c4085240b95e795f692c4cec4d805f8a"
uuid = "aed1982a-8fda-507f-9586-7b0439959a61"
version = "1.1.34+0"

[[Xorg_libX11_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libxcb_jll", "Xorg_xtrans_jll"]
git-tree-sha1 = "5be649d550f3f4b95308bf0183b82e2582876527"
uuid = "4f6342f7-b3d2-589e-9d20-edeb45f2b2bc"
version = "1.6.9+4"

[[Xorg_libXau_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "4e490d5c960c314f33885790ed410ff3a94ce67e"
uuid = "0c0b7dd1-d40b-584c-a123-a41640f87eec"
version = "1.0.9+4"

[[Xorg_libXcursor_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libXfixes_jll", "Xorg_libXrender_jll"]
git-tree-sha1 = "12e0eb3bc634fa2080c1c37fccf56f7c22989afd"
uuid = "935fb764-8cf2-53bf-bb30-45bb1f8bf724"
version = "1.2.0+4"

[[Xorg_libXdmcp_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "4fe47bd2247248125c428978740e18a681372dd4"
uuid = "a3789734-cfe1-5b06-b2d0-1dd0d9d62d05"
version = "1.1.3+4"

[[Xorg_libXext_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libX11_jll"]
git-tree-sha1 = "b7c0aa8c376b31e4852b360222848637f481f8c3"
uuid = "1082639a-0dae-5f34-9b06-72781eeb8cb3"
version = "1.3.4+4"

[[Xorg_libXfixes_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libX11_jll"]
git-tree-sha1 = "0e0dc7431e7a0587559f9294aeec269471c991a4"
uuid = "d091e8ba-531a-589c-9de9-94069b037ed8"
version = "5.0.3+4"

[[Xorg_libXi_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libXext_jll", "Xorg_libXfixes_jll"]
git-tree-sha1 = "89b52bc2160aadc84d707093930ef0bffa641246"
uuid = "a51aa0fd-4e3c-5386-b890-e753decda492"
version = "1.7.10+4"

[[Xorg_libXinerama_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libXext_jll"]
git-tree-sha1 = "26be8b1c342929259317d8b9f7b53bf2bb73b123"
uuid = "d1454406-59df-5ea1-beac-c340f2130bc3"
version = "1.1.4+4"

[[Xorg_libXrandr_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libXext_jll", "Xorg_libXrender_jll"]
git-tree-sha1 = "34cea83cb726fb58f325887bf0612c6b3fb17631"
uuid = "ec84b674-ba8e-5d96-8ba1-2a689ba10484"
version = "1.5.2+4"

[[Xorg_libXrender_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libX11_jll"]
git-tree-sha1 = "19560f30fd49f4d4efbe7002a1037f8c43d43b96"
uuid = "ea2f1a96-1ddc-540d-b46f-429655e07cfa"
version = "0.9.10+4"

[[Xorg_libpthread_stubs_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "6783737e45d3c59a4a4c4091f5f88cdcf0908cbb"
uuid = "14d82f49-176c-5ed1-bb49-ad3f5cbd8c74"
version = "0.1.0+3"

[[Xorg_libxcb_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "XSLT_jll", "Xorg_libXau_jll", "Xorg_libXdmcp_jll", "Xorg_libpthread_stubs_jll"]
git-tree-sha1 = "daf17f441228e7a3833846cd048892861cff16d6"
uuid = "c7cfdc94-dc32-55de-ac96-5a1b8d977c5b"
version = "1.13.0+3"

[[Xorg_xtrans_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "79c31e7844f6ecf779705fbc12146eb190b7d845"
uuid = "c5fb5394-a638-5e4d-96e5-b29de1b5cf10"
version = "1.4.0+3"

[[Zlib_jll]]
deps = ["Libdl"]
uuid = "83775a58-1f1d-513f-b197-d71354ab007a"

[[isoband_jll]]
deps = ["Libdl", "Pkg"]
git-tree-sha1 = "a1ac99674715995a536bbce674b068ec1b7d893d"
uuid = "9a68df92-36a6-505f-a73e-abb412b6bfb4"
version = "0.2.2+0"

[[libass_jll]]
deps = ["Artifacts", "Bzip2_jll", "FreeType2_jll", "FriBidi_jll", "HarfBuzz_jll", "JLLWrappers", "Libdl", "Pkg", "Zlib_jll"]
git-tree-sha1 = "5982a94fcba20f02f42ace44b9894ee2b140fe47"
uuid = "0ac62f75-1d6f-5e53-bd7c-93b484bb37c0"
version = "0.15.1+0"

[[libfdk_aac_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "daacc84a041563f965be61859a36e17c4e4fcd55"
uuid = "f638f0a6-7fb0-5443-88ba-1cc74229b280"
version = "2.0.2+0"

[[libpng_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Zlib_jll"]
git-tree-sha1 = "94d180a6d2b5e55e447e2d27a29ed04fe79eb30c"
uuid = "b53b4c65-9356-5827-b1ea-8c7a1a84506f"
version = "1.6.38+0"

[[libvorbis_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Ogg_jll", "Pkg"]
git-tree-sha1 = "c45f4e40e7aafe9d086379e5578947ec8b95a8fb"
uuid = "f27f6e37-5d2b-51aa-960f-b287f2bc3b7a"
version = "1.3.7+0"

[[nghttp2_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "8e850ede-7688-5339-a07c-302acd2aaf8d"

[[p7zip_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "3f19e933-33d8-53b3-aaab-bd5110c3b7a0"

[[protoc_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "89b92b537ffde09cab61ad20636da135d0791007"
uuid = "c7845625-083e-5bbe-8504-b32d602b7110"
version = "3.15.6+0"

[[x264_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "4fea590b89e6ec504593146bf8b988b2c00922b2"
uuid = "1270edf5-f2f9-52d2-97e9-ab00b5d0237a"
version = "2021.5.5+0"

[[x265_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "ee567a171cce03570d77ad3a43e90218e38937a9"
uuid = "dfaa095f-4041-5dcd-9319-2fabd8486b76"
version = "3.5.0+0"
"""

# ╔═╡ Cell order:
# ╟─be0e40d1-b744-42ae-82c5-b834cffc4436
# ╟─f145d340-9b31-11eb-3c29-993174d1c291
# ╟─8533930e-8915-4e8f-9011-a9eccc197a96
# ╠═28bf9277-ce4c-4fc0-abe8-65a7ad83685b
# ╠═59b3ba48-e355-48e5-8f4e-a6cbd6821f61
# ╟─beeb5b68-adc9-48c6-b827-a70cbfddf9e5
# ╠═73472c51-067b-4053-85e5-0b0661468b43
# ╟─e2b23d40-246d-44a1-ac66-04f56422f94e
# ╠═b7d3a84b-d004-4556-a0d7-5ce9dae7583f
# ╟─aba2fffb-90fb-474a-a557-d5b0f7dab0fb
# ╟─99e34287-5df2-46a9-a7e8-1b709b22a449
# ╟─34d3d027-2b13-461c-b92c-f5a5c41c2d41
# ╠═ebacac04-ec0b-44e2-a44b-4459469e097c
# ╟─f2a53d7d-5e9f-4f94-80b6-792164e09c23
# ╟─9cf4fe5a-5e92-4395-8b63-8dc7216123b3
# ╟─d45a254d-bf40-45b5-8cef-0c93e2c3d69a
# ╟─d4f79f93-704a-47a6-b7ef-0609601ba0e0
# ╠═24a1b919-71f3-48dc-9ced-6bbb489a9b8b
# ╟─78ff512f-c5b9-4448-b9b0-a93270c810fb
# ╟─0b18b161-c2fa-4e6c-8975-c300d9c793f7
# ╠═cea0fa3d-9b9a-480e-a6b6-cda5ac870985
# ╟─0811c42b-e0fd-4788-a886-54a5df2c5915
# ╟─acbc5227-8e58-4583-924c-a36dd32b3f56
# ╟─2c75e804-653f-4d37-b54c-cd7c60a42e91
# ╠═6572ad81-4c51-4bf0-8383-f9b49fade795
# ╟─3fcc753d-e261-46b6-8f31-12dab98a5bc1
# ╠═fcf6e92b-fd5b-4b11-89b6-74a88289add3
# ╟─aed9eb04-7374-4a2f-ace4-42b14d574120
# ╟─98693f6c-015a-4410-a386-f6e1659d6491
# ╟─23298957-3c0c-420b-a1bc-84438882db08
# ╟─3e167001-1d04-4b82-8657-492e9fc4566b
# ╟─25498ab7-4dd4-4074-83c9-cc71bfdc9794
# ╟─168fc810-5cf7-412a-b732-f0543ad27fd5
# ╟─bb2f5853-bc46-4171-a2c0-ee8abf90282c
# ╟─00000000-0000-0000-0000-000000000001
# ╟─00000000-0000-0000-0000-000000000002
