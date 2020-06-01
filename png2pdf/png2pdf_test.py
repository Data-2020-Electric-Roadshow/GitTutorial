# -*- coding: utf-8 -*-
"""
Created on Sat May 30 10:18:34 2020

@author: Lenovo
"""
import os
import io
import json
import cv2
import requests

os.chdir("E:\\Data Plus\\data\\raw\\From REA\\Annual Statistical Report")

f = open('url_list.txt','w')
    
for page in range(17,18):
    img = cv2.imread('page_new_'+str(page)+'.png')

    url_api = "https://api.ocr.space/parse/image"
    _, compressedimage = cv2.imencode(".png", img, [cv2.IMWRITE_PNG_COMPRESSION, 9])
    file_bytes = io.BytesIO(compressedimage)

    result = requests.post(url_api,
              files = {'page_new_'+str(page)+'.png': file_bytes},
              data = {"apikey": "01a5745a1a88957",
                      "language": "eng",
                      "isCreateSearchablePdf": True,
                      "isSearchablePdfHideTextLayer": True,
                      "detectOrientation": True,
                      "isTable": True,
                      "scale": True})

    result = result.content.decode()
    result = json.loads(result)

    parsed_url = result.get("SearchablePDFURL")

    pdfResp = requests.get(parsed_url, stream = True)
    with open('output_{}.pdf'.format(page), 'wb') as pdfFile:
      pdfFile.write(pdfResp.content)

    print('This is the result of: '+str(page))
    print(parsed_url)
    
    f.write(parsed_url)
    f.write('\n')
    
f.close()