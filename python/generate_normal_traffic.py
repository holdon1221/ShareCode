# To control the carla simulator
import carla
# To get the input from shell
from argparse import ArgumentParser
import logging, random, time
import numpy as np
import matplotlib.pyplot as plt
# To get movie
import cv2
import os
import sys
import json

def get_args():
    argparser = ArgumentParser(description=__doc__)
    argparser.add_argument(
        '--map',
        metavar='M',
        default='Town10HD',
        help='Load the map named here (default: Town10HD)'
    )
    argparser.add_argument(
        '--host',
        metavar='H',
        default='localhost',
        help='IP of the host server (default: localhost)'
    )
    argparser.add_argument(
        '-p', '--port',
        metavar='P',
        default=2000,
        type=int,
        help='TCP port to listen to (default: 2000)'
    )
    argparser.add_argument(
        '-n', '--number-of-vehicles',
        metavar='N',
        default=10,
        type=int,
        help='Number of vehicles (default: 10)'
    )
    argparser.add_argument(
        '-T', '--simulation-time',
        metavar='T',
        default=60.0,
        type=float,
        help='Total simulation time (seconds, default: 60.0)'
    )
    argparser.add_argument(
        '-w', '--number-of-walkers',
        metavar='W',
        default=50,
        type=int,
        help='Number of walkers (default: 50)'
    )
    argparser.add_argument(
        '--safe',
        action='store_true',
        help='Avoid spawnning vehicles prone to accidents'
    )
    argparser.add_argument(
        '--generationv',
        metavar='G',
        default='2',
        help='restrict to certain vehicle generation (values: "1", "2", "All" - default: "2")'
    )
    argparser.add_argument(
        '--generationw',
        metavar='G',
        default='2',
        help='restrict to certain pedestrian generation (values: "1","2","All" - default: "2")'
    )
    argparser.add_argument(
        '--tm-port',
        metavar='P',
        default=8000,
        type=int,
        help='Port to communicate with TM (default: 8000)'
    )
    argparser.add_argument(
        '--acceleration',
        action='store_true',
        help='Get acceleration graph for all vehicles'
    )
    argparser.add_argument(
        '-t', '--dt',
        default=0.05,
        type=float,
        help='\delta t for synchronous simulation'
    )
    argparser.add_argument(
        '--break-frames',
        default=6000,
        type=int,
        help='The number of frames breaking generation of sensor data'
    )
    argparser.add_argument(
        '--data-collect',
        action='store_true',
        help='Get sensor data for the simulation'
    )
    
    argparser.add_argument(
        '--recording',
        action='store_true',
        help='Get recording file from simulation'
    )
    argparser.add_argument(
        '--data-collect-movie',
        action='store_true',
        help='Get sensor data and movie for the simulation'
    )
    argparser.add_argument(
        '--rgb-front',
        action='store_true',
        help='Attach RGB camera sensor at the front'
    )
    argparser.add_argument(
        '--rgb-right',
        action='store_true',
        help='Attach RGB camera sensor at the right'
    )
    argparser.add_argument(
        '--rgb-left',
        action='store_true',
        help='Attach RGB camera sensor at the left'
    )
    argparser.add_argument(
        '--rgb-rear',
        action='store_true',
        help='Attach RGB camera sensor at the rear'
    )
    argparser.add_argument(
        '--lidar',
        action='store_true',
        help='Attach LiDAR sensor at the vehicle'
    )
    argparser.add_argument(
        '--bev',
        action='store_true',
        help='Attach BEV sensor at the vehicle'
    )
    argparser.add_argument(
        '--asynch',
        action='store_true',
        help='Activate asynchronous mode execution'
    )
    argparser.add_argument(
        '-s', '--seed',
        metavar='S',
        default=0,
        type=int,
        help='Set random device seed and deterministic mode for Traffic Manager'
    )
    argparser.add_argument(
        '--seedw',
        metavar='S',
        default=0,
        type=int,
        help='Set the meed for pedestrians module'
    )
    argparser.add_argument(
        '--car-lights-on',
        metavar='L',
        default=True,
        type=bool,
        help='Automatic car light management'
    )
    args = argparser.parse_args()

    return args

def get_basic_components_from_carla(args):
    client = carla.Client(args.host, args.port)
    client.set_timeout(100.0)

    client.load_world(args.map)
    world = client.get_world()
    random.seed(args.seed if args.seed is not None else int(time.time()))
    
    # Traffic manager
    traffic_manager = client.get_trafficmanager(args.tm_port)

    settings = world.get_settings()
    if not args.asynch:
        traffic_manager.set_synchronous_mode(True)
        settings.synchronous_mode = True
        settings.fixed_delta_seconds = args.dt
    else:
        print("\nYou are currently in asynchronous mode. If this is a traffic simulation, \
        \nyou could experience some issues. If it's not working correctly, switch to synchronous \
        \nmode by using traffic_manager.set_synchronous_mode(True)")

    world.apply_settings(settings)
    
    return client, world, traffic_manager

def spawn_vehicles(args, basic_components):
    _, world, traffic_manager = basic_components

    vehicle_list = []

    # Set spawn points
    spawn_points = world.get_map().get_spawn_points()
    number_of_spawn_points = len(spawn_points)
    if args.number_of_vehicles <= number_of_spawn_points:
        random.shuffle(spawn_points)
    elif args.number_of_vehicles > number_of_spawn_points:
        msg = 'requested %d vehicles, but could only find %d spawn points (maximum available vehicles)'
        logging.warning(msg, args.number_of_vehicles, number_of_spawn_points)
        args.number_of_vehicles = number_of_spawn_points

    # Spwan vehicles 4 wheels and light available only.
    car_bps = world.get_blueprint_library().filter("vehicle.*").filter_by_attribute("number_of_wheels", "4")
    if args.car_lights_on:
        car_bps = [vc for vc in car_bps 
                  if vc.get_attribute("has_lights").as_bool()]
    
    num_fail = 0
    for n, spawn_point in enumerate(spawn_points):
        if n >= args.number_of_vehicles:
            break

        car_bp = random.choice(car_bps)
        # Randomize car color
        if car_bp.has_attribute('color'):
            color = random.choice(car_bp.get_attribute('color').recommended_values)
            car_bp.set_attribute('color', color)

        # Spawn vehicles
        vehicle = world.try_spawn_actor(car_bp, spawn_point)
        if vehicle is not None:
            vehicle_list.append(
                vehicle
            )
        else:
            num_fail += 1

    for n, spawn_point in enumerate(spawn_points[args.number_of_vehicles:]):
        if n >= num_fail:
            break

        car_bp = random.choice(car_bps)
        # Randomize car color
        if car_bp.has_attribute('color'):
            color = random.choice(car_bp.get_attribute('color').recommended_values)
            car_bp.set_attribute('color', color)

        # Spawn vehicles
        vehicle = world.try_spawn_actor(car_bp, spawn_point)
        vehicle_list.append(
            vehicle
        )

    # Set the traffic manager with low probability of accidents
    for vehicle in vehicle_list:
        vehicle.set_autopilot(True, traffic_manager.get_port())
        traffic_manager.auto_lane_change(vehicle, True) 
        if args.car_lights_on:
            traffic_manager.update_vehicle_lights(vehicle, True)
    # Set the distance from the leading vehicle
    traffic_manager.set_global_distance_to_leading_vehicle(2.3)
    # Max speed
    traffic_manager.global_percentage_speed_difference(46.0)

    return vehicle_list

def spawn_walkers(args, basic_components):
    _, world, _ = basic_components

    walker_list = []
    
    probability_pedestrians_run = 0.0
    probability_pedestrians_cross = 0.0
    
    if args.seedw:
        world.set_pedestrians_seed(args.seedw)
        random.seed(args.seedw)
    
    spawn_points = []
    for i in range(args.number_of_walkers):
        spawn_point = carla.Transform()
        loc = world.get_random_location_from_navigation() +carla.Location(z=1)
        if (loc != None):
            spawn_point.location = loc
            spawn_points.append(spawn_point)
    
    walker_speed = []
    walker_bps = world.get_blueprint_library().filter("walker.*")
    for spawn_point in spawn_points:
        walker_bp = random.choice(walker_bps)
        # Set as not invincible
        if walker_bp.has_attribute("is_invincible"):
            walker_bp.set_attribute("is_invincible", "False")
        
        # Set the maximum speed
        if walker_bp.has_attribute("speed"):
            # Set the speed variables
            speed_stop, speed_walk, speed_run = walker_bp.get_attribute("speed").recommended_values
            
            if random.random() > probability_pedestrians_run:
                # Walk
                walker_speed.append(speed_walk)
            else:
                # Run
                walker_speed.append(speed_run)
        else:
            print("Walker has no speed")
            walker_speed.append(0.0)
        
        walker_list.append(
            world.try_spawn_actor(walker_bp, spawn_point)
        )
        
    # ------------------------------------#
    #       Spawn walker controller       #
    # ------------------------------------#
    # Developing.. Walker control AI (Incomplete)
    walker_controller_bp = world.get_blueprint_library().find('controller.ai.walker')

    walker_controller_list = []
    world.set_pedestrians_cross_factor(probability_pedestrians_cross)

    for i in range(len(walker_list)):
        controller = world.spawn_actor(walker_controller_bp, carla.Transform(), attach_to=walker_list[i])
        walker_controller_list.append(controller)

    for controller in walker_controller_list:
        controller.start()
        controller.go_to_location(world.get_random_location_from_navigation())
        controller.set_max_speed(float(walker_speed[i]))

    return walker_list, walker_controller_list

def get_acceleration_all_vehicles(basic_components, vehicle_list, ticks):
    _, world, _ = basic_components

    acceleartion_list = {v.id: [] for v in vehicle_list}

    # Get all accelerations
    for _ in range(ticks):
        world.tick()
        for v in vehicle_list:
            acceleartion_list[v.id].append(
                v.get_acceleration().squared_length()**(1/2)
            )

    return acceleartion_list

def draw_acceleration_all_vehicle(args, basic_components, vehicle_list):
    total_time = int(args.simulation_time/args.dt)
    ac_list = get_acceleration_all_vehicles(basic_components, vehicle_list, total_time)
    # Time axis (--> seconds)
    time_axis = np.arange(total_time) * args.dt 

    # Find the maximum acceleration value.
    max_acceleration_per_frame = np.max(list(ac_list.values()), axis=0)

    # Plotting
    plt.figure(figsize=(14, 8))
    for vehicle_id, accelerations in ac_list.items():
        # For individual vehicles, I set it lighter color
        plt.scatter(time_axis, accelerations, color='lightgray',s=0.5)

    # For maximum acceleration, set it saturated color
    plt.scatter(time_axis, max_acceleration_per_frame, color='black', s=3)

    plt.xlabel('Time (seconds)')
    plt.ylabel('Acceleration ($m/s^2$)')
    plt.legend()

    plt.grid(True)
    plt.show()

def set_sensors(args, basic_components, vehicle_list, file_path):
    _, world, _ = basic_components

    cars = []
    for _vc in vehicle_list:
        if _vc.attributes['base_type'] == 'car':
            cars.append(_vc)
    vc = cars[2]

    sensors = [
        {# 1200 900 (LMDrive)
            "type": "sensor.camera.rgb", "width": 800, "height": 600, 
            "x": 1.3, "y": 0.0, "z": 2.3,
            "roll": 0.0, "pitch": 0.0, "yaw": 0.0, 
            "fov": 100, "id": "rgb_front",
        },
        {
            "type": "sensor.camera.rgb", "width": 800, "height": 600, 
            "x": 1.3, "y": 0.0, "z": 2.3,
            "roll": 0.0, "pitch": 0.0, "yaw": -60.0,
            "fov": 100, "id": "rgb_left",
        },
        {
            "type": "sensor.camera.rgb", "width": 800, "height": 600,
            "x": 1.3, "y": 0.0, "z": 2.3,
            "roll": 0.0, "pitch": 0.0, "yaw": 60.0,
            "fov": 100, "id": "rgb_right",
        },
        {
            "type": "sensor.camera.rgb", "width": 800, "height": 600,
            "x": 1.3, "y": 0.0, "z": 2.3,
            "roll": 0.0, "pitch": 0.0, "yaw": 180.0,
            "fov": 100, "id": "rgb_rear",
        },
        {
            "type": "sensor.lidar.ray_cast",
            "x": 1.3, "y": 0.0, "z": 2.5,
            "roll": 0.0, "pitch": 0.0, "yaw":-90.0,
            "range": str(85.0), "rotation_frequency": str(20),
            "channels": str(64), "points_per_second": str(600000), 
            "id": "lidar",
        }
    ]

    sensor_list = []

    ##################
    #  BEV spectator #
    ##################
    if args.bev:
        bev_cam_fp = file_path+vc.type_id[8:]+"_"+str(vc.id)+"//bev"+"//"

        bev_cam_bp = world.get_blueprint_library().find(id='sensor.camera.rgb')
        bev_cam_bp.set_attribute("image_size_x", str(400))
        bev_cam_bp.set_attribute("image_size_y", str(300))

        bev_cam = world.spawn_actor(bev_cam_bp, carla.Transform())
        bev_cam.listen(lambda image: image.save_to_disk(bev_cam_fp + "%d.jpg" % image.frame))
        sensor_list.append(bev_cam)

    ##############
    #  RGB front #
    ##############
    if args.rgb_front:
        rgb_front_fp = file_path+vc.type_id[8:]+"_"+str(vc.id)+"//"+sensors[0]["id"]+"//"

        rgb_front_bp = world.get_blueprint_library().find(sensors[0]["type"])

        rgb_front_bp.set_attribute("image_size_x", str(sensors[0]["width"]))
        rgb_front_bp.set_attribute("image_size_y", str(sensors[0]["height"]))
        rgb_front_bp.set_attribute("fov", str(sensors[0]["fov"]))

        rgb_front_transform = carla.Transform(
            carla.Location(x=sensors[0]["x"], y=sensors[0]["y"], z=sensors[0]["z"]),
            carla.Rotation(roll=sensors[0]["roll"], pitch=sensors[0]["pitch"], yaw=sensors[0]["yaw"])
        )

        rgb_front = world.spawn_actor(
            rgb_front_bp,
            rgb_front_transform,
            attach_to=vc,
            attachment_type=carla.AttachmentType.Rigid
        )

        rgb_front.listen(lambda image: image.save_to_disk(rgb_front_fp + "%d.jpg" % image.frame))
        sensor_list.append(rgb_front)

    ##############
    #  RGB right #
    ##############
    if args.rgb_right:
        rgb_right_fp = file_path+vc.type_id[8:]+"_"+str(vc.id)+"//"+sensors[1]["id"]+"//"

        rgb_right_bp = world.get_blueprint_library().find(sensors[1]["type"])

        rgb_right_bp.set_attribute("image_size_x", str(sensors[1]["width"]))
        rgb_right_bp.set_attribute("image_size_y", str(sensors[1]["height"]))
        rgb_right_bp.set_attribute("fov", str(sensors[1]["fov"]))

        rgb_right_transform = carla.Transform(
            carla.Location(x=sensors[1]["x"], y=sensors[1]["y"], z=sensors[1]["z"]),
            carla.Rotation(roll=sensors[1]["roll"], pitch=sensors[1]["pitch"], yaw=sensors[1]["yaw"])
        )

        rgb_right = world.spawn_actor(
            rgb_right_bp,
            rgb_right_transform,
            attach_to=vc,
            attachment_type=carla.AttachmentType.Rigid
        )

        rgb_right.listen(lambda image: image.save_to_disk(rgb_right_fp + "%d.jpg" % image.frame))
        sensor_list.append(rgb_right)

    #############
    #  RGB left #
    #############
    if args.rgb_left:
        rgb_left_fp = file_path+vc.type_id[8:]+"_"+str(vc.id)+"//"+sensors[2]["id"]+"//"

        rgb_left_bp = world.get_blueprint_library().find(sensors[2]["type"])

        rgb_left_bp.set_attribute("image_size_x", str(sensors[2]["width"]))
        rgb_left_bp.set_attribute("image_size_y", str(sensors[2]["height"]))
        rgb_left_bp.set_attribute("fov", str(sensors[2]["fov"]))

        rgb_left_transform = carla.Transform(
            carla.Location(x=sensors[2]["x"], y=sensors[2]["y"], z=sensors[2]["z"]),
            carla.Rotation(roll=sensors[2]["roll"], pitch=sensors[2]["pitch"], yaw=sensors[2]["yaw"])
        )

        rgb_left = world.spawn_actor(
            rgb_left_bp,
            rgb_left_transform,
            attach_to=vc,
            attachment_type=carla.AttachmentType.Rigid
        )

        rgb_left.listen(lambda image: image.save_to_disk(rgb_left_fp + "%d.jpg" % image.frame))
        sensor_list.append(rgb_left)

    #############
    #  RGB rear #
    #############
    if args.rgb_rear:
        rgb_rear_fp = file_path+vc.type_id[8:]+"_"+str(vc.id)+"//"+sensors[3]["id"]+"//"

        rgb_rear_bp = world.get_blueprint_library().find(sensors[3]["type"])

        rgb_rear_bp.set_attribute("image_size_x", str(sensors[3]["width"]))
        rgb_rear_bp.set_attribute("image_size_y", str(sensors[3]["height"]))
        rgb_rear_bp.set_attribute("fov", str(sensors[3]["fov"]))

        rgb_rear_transform = carla.Transform(
            carla.Location(x=sensors[3]["x"], y=sensors[3]["y"], z=sensors[3]["z"]),
            carla.Rotation(roll=sensors[3]["roll"], pitch=sensors[3]["pitch"], yaw=sensors[3]["yaw"])
        )

        rgb_rear = world.spawn_actor(
            rgb_rear_bp,
            rgb_rear_transform,
            attach_to=vc,
            attachment_type=carla.AttachmentType.Rigid
        )

        rgb_rear.listen(lambda image: image.save_to_disk(rgb_rear_fp + "%d.jpg" % image.frame))
        sensor_list.append(rgb_rear)

    ###########
    #  LiDAR  #
    ###########
    if args.lidar:
        lidar_fp = file_path+vc.type_id[8:]+"_"+str(vc.id)+"//"+sensors[4]["id"]+"//"

        lidar_bp = world.get_blueprint_library().find(sensors[4]["type"])

        lidar_transform = carla.Transform(
            carla.Location(x=sensors[4]["x"], y=sensors[4]["y"], z=sensors[4]["z"]),
            carla.Rotation(roll=sensors[4]["roll"], pitch=sensors[4]["pitch"], yaw=sensors[4]["yaw"])
        )

        lidar_bp.set_attribute('range', sensors[4]["range"])
        lidar_bp.set_attribute('rotation_frequency', sensors[4]["rotation_frequency"])
        lidar_bp.set_attribute('channels', sensors[4]["channels"])
        lidar_bp.set_attribute('points_per_second', sensors[4]["points_per_second"])

        lidar = world.spawn_actor(
            lidar_bp,
            lidar_transform,
            attach_to=vc,
            attachment_type=carla.AttachmentType.Rigid
        )

        lidar.listen(lambda point_cloud: lidar_callback(point_cloud, lidar_fp + "%d.npy" % point_cloud.frame))
        sensor_list.append(lidar)

    def lidar_callback(data, filepath_name):
        # Lidar raw data to npy format
        points = np.frombuffer(data.raw_data, dtype=np.dtype('f4'))
        points = np.reshape(points, (int(points.shape[0] / 4), 4))

        # check the directory existence
        directory = os.path.dirname(filepath_name)
        if not os.path.exists(directory):
            os.makedirs(directory)
        
        # save npy
        np.save(filepath_name, points)

    return sensor_list, vc

################
#  Actor data  #
################
# Developing.. Actor info
def collect_actor_data(vc, world, filename):
    data = {}
    actors = world.get_actors()
    vehicles = actors.filter("vehicle.*")
    walkers = actors.filter("walkers.*")
    lights = actors.filter("traffic_light*")

    for actor in vehicles:
        location = actor.get_location()
        if location.distance(vc.get_location()) > 50:
            continue
        actor_data = collect_actor_info(actor)
        actor_data["tpe"] = 0
        data[actor.id] = actor_data
    
    for actor in walkers:
        location = actor.get_location()
        if location.distasnce(vc.get_location()) > 50:
            continue
        actor_data = collect_actor_info(actor)
        actor_data["tpe"] = 1
        data[actor.id] = actor_data

    for actor in lights:
        location = actor.get_location()
        if location.distasnce(vc.get_location()) > 70:
            continue
        actor_data = collect_actor_info(actor, traffic_light=True)
        actor_data["tpe"] = 2
        data[actor.id] = actor_data

    with open(filename, 'w') as f:
        json.dump(data, f, indent=4)

# Developing.. Actor info
def collect_actor_info(actor, traffic_light=False):
    location = actor.get_location()
    orientation = actor.get_transform().rotation.get_forward_vector()
    bbox = actor.bounding_box.extent if hasattr(actor, 'bounding_box') else carla.Vector3D()
    velocity = actor.get_velocity()

    actor_data = {
        "loc": [loc.x, loc.y, loc.z],
        "ori": [ori.x, ori.y, ori.z],
        "box": [box.x, box.y],
        "vel": [vel.x, vel.y, vel.z],
    }

    if traffic_light:
        actor_data["sta"] = int(actor.state)
        trigger = actor.trigger_volume
        actor_data["trigger_loc"] = [
            trigger.location.x, trigger.location.y, trigger.location.z
        ]
        actor_data["trigger_ori"] = [
            trigger.rotation.x, trigger.rotation.y, trigger.rotation.z
        ]
        actor_data["trigger_box"] = [
            trigger.extent.x, trigger.extent.y
        ]

    return actor_data

def check_files_count_equal(directory):
    files_count = {}  # Count the files in the directory for each sensor
    
    for root, dirs, files in os.walk(directory):
        for dir in dirs:
            dir_path = os.path.join(root, dir)
            # Count files for each sensor
            files_count[dir] = len([f for f in os.listdir(dir_path) if os.path.isfile(os.path.join(dir_path, f))])
    
    # Visualize it with progress bar
    print_progress_bar(np.min(list(files_count.values())), np.max(list(files_count.values())))

    if not files_count:  # Except
        return False
    
    # Check if all values in the files_count are the same
    return len(set(files_count.values())) == 1

def print_progress_bar(iteration, total, prefix='', suffix='', decimals=2, length=50, fill='█'):

    # Set progress
    progress = ("{0:" + str(decimals) + "d}/{0:" + str(decimals) + "d}").format(iteration, total)
    
    # How many 'fill' should be
    filled_length = int(length * iteration // total)
    bar = fill * filled_length + '-' * (length - filled_length)
    
    # System-writing
    sys.stdout.write(f'\r{prefix} |{bar}| {progress} {suffix}')
    sys.stdout.flush()
    # Print new line on complete
    if iteration == total: 
        print()

def get_data_with_various_weathers(args, basic_components, vehicle_list, file_path, get_data=False):
    client, world, traffic_manager = basic_components

    weather = [
        carla.WeatherParameters.Default,
        carla.WeatherParameters.CloudySunset,
        carla.WeatherParameters.CloudyNoon,
        carla.WeatherParameters.CloudyNight,
        carla.WeatherParameters.ClearSunset,
        carla.WeatherParameters.ClearNoon,
        carla.WeatherParameters.ClearNight, # Custom in LMDrive
        carla.WeatherParameters.HardRainSunset,
        carla.WeatherParameters.HardRainNoon,
        carla.WeatherParameters.HardRainNight, # Custom in LMDrive,
        carla.WeatherParameters.MidRainSunset,
        carla.WeatherParameters.MidRainyNoon,
        carla.WeatherParameters.MidRainyNight,
        carla.WeatherParameters.SoftRainSunset,
        carla.WeatherParameters.SoftRainNoon,
        carla.WeatherParameters.SoftRainNight,
        carla.WeatherParameters.WetSunset,
        carla.WeatherParameters.WetNoon,
        carla.WeatherParameters.WetNight,
        carla.WeatherParameters.WetCloudySunset,
        carla.WeatherParameters.WetCloudyNoon,
        carla.WeatherParameters.WetCloudyNight,
        carla.WeatherParameters.DustStorm
    ]
    kinds_of_weather = len(weather)

    # Burn in
    for _ in range(int(10/args.dt)):
        world.tick()

    if get_data:
        sensor_list, vc = set_sensors(args, basic_components, vehicle_list, file_path)
        print(f"\n Attach the sensors {[sensor.type_id for sensor in sensor_list]}")
    time.sleep(10.0)

    def on_tick(world_snapshot):
        collect_actor_data(vc, world, world_snapshot.frame)

    spectator = world.get_spectator()

    # Total simulation times --> frames
    total_frames = int(args.simulation_time / args.dt)
    # Frames for each weather; if you want to use it as LMDrive later, you should keep weather as an id.
    frames_per_weather = total_frames // kinds_of_weather
    current_weather_frame = 0
    current_weather_index = 0

    # To check the weather changes
    weather_change_needed = True

    # If need to be recorded
    if args.recording:
        os.mkdir(file_path+vc.type_id[8:]+"_"+str(vc.id))
        client.start_recorder(file_path+vc.type_id[8:]+"_"+str(vc.id)+"//recording.log", True)


    target_frames = args.break_frames 
    for tick in range(total_frames):
        # Change weather
        if weather_change_needed:
            world.set_weather(weather[current_weather_index])
            weather_change_needed = False  # No change after it
            for vehicle in vehicle_list:
                if args.car_lights_on:
                    traffic_manager.update_vehicle_lights(vehicle, False)
                    time.sleep(0.1)
                    traffic_manager.update_vehicle_lights(vehicle, True)
        
        # Progress simulation
        if args.bev:
            sensor_list[0].set_transform(
                carla.Transform(vc.get_transform().location + carla.Location(z=50),
                                carla.Rotation(pitch=-90))
            )
        spectator.set_transform(
            carla.Transform(vc.get_transform().location + carla.Location(z=50),
                            carla.Rotation(pitch=-90))
        )
        world.tick()
        time.sleep(args.dt * 4) # To keep all sensor data
        
        # Frame counter
        current_weather_frame += 1
        
        # Ready to change weather
        if current_weather_frame >= frames_per_weather:
            current_weather_frame = 0
            current_weather_index = (current_weather_index + 1) % kinds_of_weather
            weather_change_needed = True  # change weather
        
        # Check for generation of data
        if (tick + 1) % args.break_frames == 0:
            print(f"Complete the generation every {args.break_frames} frame.")
            print(f"Now checking {target_frames} files are generated for each sensor (Min/Max)...")
            while True:
                if check_files_count_equal(directory=file_path+vc.type_id[8:]+"_"+str(vc.id)):
                    break
                else:
                    time.sleep(1.0)

            target_frames += args.break_frames
            target_frames = total_frames if total_frames < target_frames else target_frames

    if args.recording:
        client.stop_recorder()

    return sensor_list


# Developing... (get movie)
def get_movie_from_rgb_data(args, vc: carla.Actor, file_path):
    # Folder
    folders = {
        "front": file_path+vc.type_id[8:]+"_"+str(vc.id)+"//rgb_front",
        "rear": file_path+vc.type_id[8:]+"_"+str(vc.id)+"//rgb_rear",
        "left": file_path+vc.type_id[8:]+"_"+str(vc.id)+"//rgb_left",
        "right": file_path+vc.type_id[8:]+"_"+str(vc.id)+"//rgb_right"
    }

    fps = int(1/args.dt)
    video_name = file_path+vc.type_id[8:]+"_"+str(vc.id)+"//movies//output_video.avi"

    # determine the shape of images
    sample_image_path = os.listdir(folders["left"])[0]
    sample_image = cv2.imread(os.path.join(folders["left"], sample_image_path))
    height, width, layers = sample_image.shape

    # Final video frames
    final_height, final_width = height * 2, width * 2

    # Set the video writer
    video = cv2.VideoWriter(video_name, cv2.VideoWriter_fourcc(*'DIVX'), fps, (final_width, final_height))

    # Sorting
    frame_files = sorted(os.listdir(folders["front"]), key=lambda x: int(os.path.splitext(x)[0]))

    for filename in frame_files:
        frames = []
        for position in ["front", "rear", "left", "right"]:
            folder = folders[position]
            img_path = os.path.join(folder, filename)
            
            if os.path.exists(img_path):
                img = cv2.imread(img_path)
                
                # Resize the image
                if position == "front":
                    img = cv2.resize(img, (width, height))
                    
                cv2.putText(img, position.capitalize(), (50, 50), cv2.FONT_HERSHEY_DUPLEX, 0.5, (255, 255, 255), 1)
                frames.append(img)

        # Set frames
        top_frame = np.hstack((frames[0], frames[1]))  # front, rear
        bottom_frame = np.hstack((frames[2], frames[3]))  # left, right
        combined_frame = np.vstack((top_frame, bottom_frame))

        # Write a frame into the video
        video.write(combined_frame)

    # Save the video and free it
    video.release()

    cv2.destroyAllWindows()

def main():
    args = get_args()
    logging.basicConfig(format='%(levelname)s: %(message)s', level=logging.INFO)

    basics = get_basic_components_from_carla(args)

    # ---------------------------#
    #       Spawn vehicles       #
    # ---------------------------#
    vehicle_list = spawn_vehicles(args, basics)

    # --------------------------#
    #       Spawn walkers       #
    # --------------------------#
    walker_list, walker_controller_list = spawn_walkers(args, basics)


    print("Spawn %d vehicles and %d walkers, press Ctrl+C to exit." % (len(vehicle_list), len(walker_list)))

    try: 
        if args.acceleration:
            draw_acceleration_all_vehicle(args, basics, vehicle_list)

        if args.data_collect or args.data_collect_movie:
            sensor_list = get_data_with_various_weathers(args, basics, vehicle_list, "C://sensors//", get_data=True)

        input("Press enter after finishing it")
    except Exception as e:
        if hasattr(e, 'message'):
            print(e.message)
        else:
            print(e)
    finally:
        for sensor in sensor_list:
            sensor.destroy()

        for v in vehicle_list:
            v.destroy()
        
        for w in walker_list:
            w.destroy()

        for wc in walker_controller_list:
            wc.stop()
            wc.destroy()

        del basics

    

if __name__ == '__main__':

    try:
        main()
    except KeyboardInterrupt:
        pass
    finally:
        print('\nDone!')