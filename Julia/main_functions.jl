include("input_data.jl")
include("make_abm.jl")
include("collect_data.jl")
include("add_agents.jl")

function create_sev_model_all_susceptible(
    β_input, is_only_am_therapy_input, test_number_per_week_input
)

    sev_traffic_data = call_traffic_data("severance")

    sev_model = initialize_model(
        place="severance",
        β=β_input,
        incubation_period_distn=24 * LogNormal(1.857, 0.547),
        presymptomatic_infectious_period_distn=24 * TruncatedNormal(2.3, 0.49, 0, Inf),
        infectious_period_distn=24 * TruncatedNormal(7.2, 4.96, 0, Inf),
        is_only_am_therapy=is_only_am_therapy_input,
        test_number_per_week=test_number_per_week_input,
        traffic_data=sev_traffic_data
    )

    # Add nurses
    n_nurses = 19 + 17 + 12
    add_n_nurses_on_floor_whos_status!(;
        model=sev_model, number=19, main_floor=7, status=:S
    )
    add_n_nurses_on_floor_whos_status!(;
        model=sev_model, number=17, main_floor=8, status=:S
    )
    add_n_nurses_on_floor_whos_status!(;
        model=sev_model, number=12, main_floor=10, status=:S
    )

    # Add transferers
    n_transferers = 2 + 6 + 2 + 2
    add_n_transfers_on_floor_whos_status!(;
        model=sev_model, number=2, main_floor=7, status=:S
    )
    add_n_transfers_on_floor_whos_status!(;
        model=sev_model, number=6, main_floor=8, status=:S
    )
    add_n_transfers_on_floor_whos_status!(;
        model=sev_model, number=2, main_floor=9, status=:S
    )
    add_n_transfers_on_floor_whos_status!(;
        model=sev_model, number=2, main_floor=10, status=:S
    )

    # Add patients and caregivers
    n_patients =  8*4+8*6+8*2+2
    n_caregivers = n_patients
    # 7th floor
    add_n_patients_on_floor_at_room_whos_status!(;
        model=sev_model, number=8, main_floor=7, room_number=1, status=:S
    )
    add_n_caregivers_on_floor_at_room_whos_status!(;
        model=sev_model, number=8, main_floor=7, room_number=1, status=:S
    )
    for rn = 2:4
        add_n_patients_on_floor_at_room_whos_status!(;
            model=sev_model, number=8, main_floor=7, room_number=rn, status=:S
        )
        add_n_caregivers_on_floor_at_room_whos_status!(;
            model=sev_model, number=8, main_floor=7, room_number=rn, status=:S
        )
    end
    # 8th floor
    for rn = 1:6
        add_n_patients_on_floor_at_room_whos_status!(;
            model=sev_model, number=8, main_floor=8, room_number=rn, status=:S
        )
        add_n_caregivers_on_floor_at_room_whos_status!(;
            model=sev_model, number=8, main_floor=8, room_number=rn, status=:S
        )
    end
    # 10th floor
    for rn = 1:2
        add_n_patients_on_floor_at_room_whos_status!(;
            model=sev_model, number=8, main_floor=10, room_number=rn, status=:S
        )
        add_n_caregivers_on_floor_at_room_whos_status!(;
            model=sev_model, number=8, main_floor=10, room_number=rn, status=:S
        )
    end
    add_n_patients_on_floor_at_room_whos_status!(;
        model=sev_model, number=2, main_floor=10, room_number=3, status=:S
    )
    add_n_caregivers_on_floor_at_room_whos_status!(;
        model=sev_model, number=2, main_floor=10, room_number=3, status=:S
    )

    # Add operational therapists
    n_operational_therapists = 20
    add_n_operational_therapists_whos_status_at_6th!(;
        model=sev_model, number=n_operational_therapists, status=:S
    )

    n_operational_therapists = 3
    add_n_operational_therapists_whos_status_at_9th!(;
        model=sev_model, number=n_operational_therapists, status=:S
    )

    # Add robotics therapists
    n_robotic_therapists = 6
    add_n_robotic_therapists_whos_status!(;
        model=sev_model, number=n_robotic_therapists, status=:S
    )

    # Add physical therapists
    n_physical_therapists = 26
    add_n_physical_therapists_whos_status!(;
        model=sev_model, number=n_physical_therapists, status=:S
    )

    # Add cleaners
    n_cleaners = 1 + 2 + 2 + 1 + 2
    add_n_cleaners_on_floor_whos_status!(;
        model=sev_model, number=1, main_floor=6, status=:S
    )
    add_n_cleaners_on_floor_whos_status!(;
        model=sev_model, number=2, main_floor=7, status=:S
    )
    add_n_cleaners_on_floor_whos_status!(;
        model=sev_model, number=2, main_floor=8, status=:S
    )
    add_n_cleaners_on_floor_whos_status!(;
        model=sev_model, number=1, main_floor=9, status=:S
    )
    add_n_cleaners_on_floor_whos_status!(;
        model=sev_model, number=2, main_floor=10, status=:S
    )

    return sev_model
end

function calculate_infected_people(
    sev_model
)
    infectious_id = 0
    # Assume that there's only one infectious person.
    for a ∈ allagents(sev_model)
        (a.status == :I) && (infectious_id = a.id)
    end

    # define the simulation time
    simulation_time = sev_model[infectious_id].infectious_period

    # simulation
    is_initial_infected(a) = (a.id == infectious_id)
    to_collected = [(:infect_by_me, sum, is_initial_infected)] # because it is mutable, the result is same for all steps

    T = simulation_time
    rst, _ = run!(sev_model, hospital_agent_step!, hospital_model_step!, T; adata=to_collected)

    num_of_infecting_others = length(rst[end, end])

    return num_of_infecting_others
end

function calculate_Ro_total(
    nos, β_input, is_only_am_therapy_input, test_number_per_week_input
)
    NUM_OF_SIMULATION = nos

    Ro_values = -ones(nos)

    Threads.@threads for i in 1:NUM_OF_SIMULATION
        sev_model = create_sev_model_all_susceptible(
            β_input, is_only_am_therapy_input, test_number_per_week_input
        )
        initial_infected_agent = random_agent(sev_model)
        sev_model[initial_infected_agent.id].status = :I

        Ro_values[i] = calculate_infected_people(sev_model)
    end

    println("The case overall people is done.")

    return Ro_values
end


function calculate_Ro_total(
    nos, β_input, is_only_am_therapy_input, test_number_per_week_input,
    occupation
)
    NUM_OF_SIMULATION = nos

    Ro_values = -ones(nos)

    Threads.@threads for i in 1:NUM_OF_SIMULATION
        sev_model = create_sev_model_all_susceptible(
            β_input, is_only_am_therapy_input, test_number_per_week_input
        )
        initial_infected_agent =
            random_agent(sev_model, a -> a.occupation == occupation)
        sev_model[initial_infected_agent.id].status = :I

        Ro_values[i] = calculate_infected_people(sev_model)
    end

    println("The case for ", occupation, " is done.")

    return Ro_values
end

function calculate_Ro_total(
    nos, β_input, is_only_am_therapy_input, test_number_per_week_input,
    occupation, floor
)
    NUM_OF_SIMULATION = nos

    Ro_values = -ones(nos)

    Threads.@threads for i in 1:NUM_OF_SIMULATION
        sev_model = create_sev_model_all_susceptible(
            β_input, is_only_am_therapy_input, test_number_per_week_input
        )
        initial_infected_agent =
            random_agent(sev_model, a -> (a.occupation == occupation && a.main_room[1:1] == floor))
        sev_model[initial_infected_agent.id].status = :I

        Ro_values[i] = calculate_infected_people(sev_model)
    end

    println("The case for ", occupation, " at ", floor, "+6-th floor is done.")

    return Ro_values
end

function Ro_vals_df(
    nos, β_input, is_only_am_therapy_input, test_number_per_week_input
)
    Ro_vals = calculate_Ro_total(
        nos, β_input, is_only_am_therapy_input, test_number_per_week_input
    )

    Ro_vals_N = calculate_Ro_total(
        nos, β_input, is_only_am_therapy_input, test_number_per_week_input,
        "N"
    )
    Ro_vals_N7 = calculate_Ro_total(
        nos, β_input, is_only_am_therapy_input, test_number_per_week_input,
        "N", "1"
    )
    Ro_vals_N8 = calculate_Ro_total(
        nos, β_input, is_only_am_therapy_input, test_number_per_week_input,
        "N", "2"
    )
    Ro_vals_N10 = calculate_Ro_total(
        nos, β_input, is_only_am_therapy_input, test_number_per_week_input,
        "N", "4"
    )

    Ro_vals_P = calculate_Ro_total(
        nos, β_input, is_only_am_therapy_input, test_number_per_week_input,
        "P"
    )
    Ro_vals_P7 = calculate_Ro_total(
        nos, β_input, is_only_am_therapy_input, test_number_per_week_input,
        "P", "1"
    )
    Ro_vals_P8 = calculate_Ro_total(
        nos, β_input, is_only_am_therapy_input, test_number_per_week_input,
        "P", "2"
    )
    Ro_vals_P10 = calculate_Ro_total(
        nos, β_input, is_only_am_therapy_input, test_number_per_week_input,
        "P", "4"
    )

    Ro_vals_C = calculate_Ro_total(
        nos, β_input, is_only_am_therapy_input, test_number_per_week_input,
        "C"
    )
    Ro_vals_C7 = calculate_Ro_total(
        nos, β_input, is_only_am_therapy_input, test_number_per_week_input,
        "C", "1"
    )
    Ro_vals_C8 = calculate_Ro_total(
        nos, β_input, is_only_am_therapy_input, test_number_per_week_input,
        "C", "2"
    )
    Ro_vals_C10 = calculate_Ro_total(
        nos, β_input, is_only_am_therapy_input, test_number_per_week_input,
        "C", "4"
    )

    Ro_vals_Wa = calculate_Ro_total(
        nos, β_input, is_only_am_therapy_input, test_number_per_week_input,
        "Wa"
    )
    Ro_vals_Wa7 = calculate_Ro_total(
        nos, β_input, is_only_am_therapy_input, test_number_per_week_input,
        "Wa", "1"
    )
    Ro_vals_Wa8 = calculate_Ro_total(
        nos, β_input, is_only_am_therapy_input, test_number_per_week_input,
        "Wa", "2"
    )
    Ro_vals_Wa9 = calculate_Ro_total(
        nos, β_input, is_only_am_therapy_input, test_number_per_week_input,
        "Wa", "3"
    )
    Ro_vals_Wa10 = calculate_Ro_total(
        nos, β_input, is_only_am_therapy_input, test_number_per_week_input,
        "Wa", "4"
    )

    Ro_vals_Wc = calculate_Ro_total(
        nos, β_input, is_only_am_therapy_input, test_number_per_week_input,
        "Wc"
    )
    Ro_vals_Wc6 = calculate_Ro_total(
        nos, β_input, is_only_am_therapy_input, test_number_per_week_input,
        "Wc", "0"
    )
    Ro_vals_Wc7 = calculate_Ro_total(
        nos, β_input, is_only_am_therapy_input, test_number_per_week_input,
        "Wc", "1"
    )
    Ro_vals_Wc8 = calculate_Ro_total(
        nos, β_input, is_only_am_therapy_input, test_number_per_week_input,
        "Wc", "2"
    )
    Ro_vals_Wc9 = calculate_Ro_total(
        nos, β_input, is_only_am_therapy_input, test_number_per_week_input,
        "Wc", "3"
    )
    Ro_vals_Wc10 = calculate_Ro_total(
        nos, β_input, is_only_am_therapy_input, test_number_per_week_input,
        "Wc", "4"
    )

    Ro_vals_Wo = calculate_Ro_total(
        nos, β_input, is_only_am_therapy_input, test_number_per_week_input,
        "Wo"
    )
    Ro_vals_Wo6 = calculate_Ro_total(
        nos, β_input, is_only_am_therapy_input, test_number_per_week_input,
        "Wo", "0"
    )
    Ro_vals_Wo9 = calculate_Ro_total(
        nos, β_input, is_only_am_therapy_input, test_number_per_week_input,
        "Wo", "3"
    )

    Ro_vals_Wr = calculate_Ro_total(
        nos, β_input, is_only_am_therapy_input, test_number_per_week_input,
        "Wr"
    )
    Ro_vals_Wp = calculate_Ro_total(
        nos, β_input, is_only_am_therapy_input, test_number_per_week_input,
        "Wp"
    )

    Ro_df = DataFrame(
        Ro_overall=Ro_vals,
        Ro_N=Ro_vals_N,
        Ro_N7=Ro_vals_N7,
        Ro_N8=Ro_vals_N8,
        Ro_N10=Ro_vals_N10,
        Ro_P=Ro_vals_P,
        Ro_P7=Ro_vals_P7,
        Ro_P8=Ro_vals_P8,
        Ro_P10=Ro_vals_P10,
        Ro_C=Ro_vals_C,
        Ro_C7=Ro_vals_C7,
        Ro_C8=Ro_vals_C8,
        Ro_C10=Ro_vals_C10,
        Ro_Wa=Ro_vals_Wa,
        Ro_Wa7=Ro_vals_Wa7,
        Ro_Wa8=Ro_vals_Wa8,
        Ro_Wa9=Ro_vals_Wa9,
        Ro_Wa10=Ro_vals_Wa10,
        Ro_Wc=Ro_vals_Wc,
        Ro_Wc6=Ro_vals_Wc6,
        Ro_Wc7=Ro_vals_Wc7,
        Ro_Wc8=Ro_vals_Wc8,
        Ro_Wc9=Ro_vals_Wc9,
        Ro_Wc10=Ro_vals_Wc10,
        Ro_Wo=Ro_vals_Wo,
        Ro_Wo6=Ro_vals_Wo6,
        Ro_Wo9=Ro_vals_Wo9,
        Ro_Wr=Ro_vals_Wr,
        Ro_Wp=Ro_vals_Wp
    )

    CSV.write("Ro_values_occupation_specific" * "_p" * string(β_input) * ".csv", Ro_df,
        header=true)

    return Ro_df
end