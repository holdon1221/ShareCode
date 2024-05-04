import carla
import time

client = carla.Client('localhost', 2000)
client.set_timeout(30.0)  # 서버 연결 타임아웃 설정

client.load_world('Town05')
world = client.get_world()

map = world.get_map()
waypoints = map.generate_waypoints(distance=1.0)  # 각 웨이포인트 사이의 거리 설정

for waypoint in waypoints:
    # 현재 웨이포인트에서 다음 웨이포인트로 이동 가능한 모든 경로를 찾음
    next_waypoints = waypoint.next(1.0)  # 1.0 미터 앞의 웨이포인트를 찾음
    for next_waypoint in next_waypoints:
        # 현재 웨이포인트에서 다음 웨이포인트까지 선을 그림
        world.debug.draw_line(waypoint.transform.location, next_waypoint.transform.location,#+carla.Location(z=1),
                              thickness=0.5, color=carla.Color(r=0, g=0, b=255), life_time=-1.0, persistent_lines=True)

time.sleep(10)