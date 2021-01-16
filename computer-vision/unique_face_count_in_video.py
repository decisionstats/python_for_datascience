import cv2
face_cascade = cv2.CascadeClassifier('/home/spartan/Desktop/opencv-master/data/haarcascades/haarcascade_frontalface_default.xml')

from skimage.measure import compare_ssim,compare_mse

#used mse to compare two images but there are also many other options available at http://scikit-image.org/docs/dev/api/skimage.measure.html
import math

videoFile = "faces.mp4"
vidcap = cv2.VideoCapture(videoFile)
success,image = vidcap.read()
previousframe=None



seconds = 1
fps = vidcap.get(cv2.CAP_PROP_FPS) 
print(fps)
multiplier = round(fps * seconds)
print(multiplier)
faces = face_cascade.detectMultiScale(image, 1.3, 5)
count=len(faces)
while success:
    if(previousframe is None):
        previousframe=image
    frameId = int(round(vidcap.get(1))) 
    success,image = vidcap.read()
    
    if frameId % multiplier == 0:
        score=compare_mse(image,previousframe)
        if(score>4000):
            faces=face_cascade.detectMultiScale(image, 1.3, 5)
            print (len(faces))
            print(frameId)
            count=count+len(faces)
        previousframe=image    
        cv2.putText(image,('%d unique faces found till now' % count),(250,300), cv2.FONT_HERSHEY_SIMPLEX,1, (200,255,155), 2)
        
        cv2.imshow('ip',image)
        cv2.waitKey(30)
        cv2.imwrite("/home/spartan/nn/frame%d.jpg" % frameId, image)

vidcap.release()
print ("Complete")
