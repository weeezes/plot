from moviepy.editor import *
import moviepy.video.tools.drawing as dw

clip = VideoFileClip("plot_demo.mp4").set_pos(("left","bottom"))
size = clip.size
clip = clip.crop(x1=0,y1=0,x2=size[0]-2,y2=size[1])

bar = ImageClip("bar.png").to_RGB().set_pos(("left","top")).set_duration(clip.duration)

final_size = (clip.size[0],clip.size[1]+bar.size[1])

final = CompositeVideoClip([bar,clip],size=final_size)
final.write_gif("plot_demo.gif", program="ImageMagick", opt="optimizeplus")
#final.write_videofile("plot_demo_bar.mp4", fps=10, audio=False)