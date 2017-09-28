# -*- coding: utf-8 -*-
"""
Created on Wed Sep 20 00:58:02 2017

@author: Godnov
"""

"""
Created on Sun Sep 10 15:00:43 2017

@author: Godnov
"""

import os
import requests
from bs4 import BeautifulSoup
from binascii import a2b_base64
from PIL import Image

def decodeImgData(url,appNo):
    r = requests.get(url)
    c = r.content
    soup = BeautifulSoup(c, "html.parser")
    imgURI=soup.img['src']
    binary_data = a2b_base64(imgURI.split(",")[1])
    imgNameGif='./logos/'+appNo+'.gif'
    fd = open(imgNameGif, 'wb')
    fd.write(binary_data)
    fd.close()
    img = Image.open(imgNameGif)
    img.save('./logos/'+appNo+'.jpg','jpeg')
    os.remove(imgNameGif)
    img.close()


