import yt_dlp
import subprocess
import os
import whisper
from google.cloud import speech_v1 as speech
import json
# import sys
import threading
import time
import asyncio
# import threading
# import time

# Configure paths and URLs
LIVE_STREAM_URL = 'https://www.youtube.com/live/yUVq6zpK5m8'  # Replace with the actual URL
TEMP_AUDIO_FILE = 'live_stream_audio.wav'
PART_FILE = 'live_stream_audio.wav.part.part'
PART_2 = 'live_stream_audio.wav.part'
TEMP_A2 = 'live_stream_audio.wav.wav'
CONVERTED_AUDIO_FILE = 'converted_audio.wav'
GOOGLE_APPLICATION_CREDENTIALS = 'path/to/your/service_account_key.json'

# Function to download live stream audio
# def download_audio(url: str, output_file: str):
#     ydl_opts = {
#         'format': 'bestaudio/best',
#         'outtmpl': output_file,
#         'noplaylist': True,
#         'quiet': True,
#         'postprocessors': [{
#             'key': 'FFmpegExtractAudio',
#             'preferredcodec': 'wav',
#             'preferredquality': '192',
#         }],
#     }
#     with yt_dlp.YoutubeDL(ydl_opts) as ydl:
#         ydl.download([url])


async def download_audio_part(url: str, output_file: str):
    part_number = 0

    for x in range(10):
        print("Downloading audio...")
        # Ensure the output file has a .part extension
        part_file = f"{output_file}{part_number}.part"
        ydl_opts = {
            'format': 'bestaudio/best',
            'outtmpl': part_file,
            'noplaylist': True,
            'quiet': True,
            'username': 'oauth2',
            'password': '',
        }
        part_number += 1
    
        with yt_dlp.YoutubeDL(ydl_opts) as ydl:
            task = asyncio.get_running_loop().run_in_executor(None, ydl.download, [url])
            try:
                await asyncio.wait_for(task, timeout=10.0)
                
            except asyncio.TimeoutError:
                task.cancel()
                print("hi")

        print("Downloaded")
        yield part_file
    


async def convert(part_file: str, output_file: str):
    def convert_part_to_wav():
        print("Converting audio pt1...")
        part_file = PART_2
        output_file = TEMP_AUDIO_FILE
        # Ensure the output file has a .wav extension
        wav_file = f"{output_file}.wav"
        # Construct the ffmpeg command
        command = [
            'ffmpeg',
            '-i', part_file,
            '-vn',  # No video
            '-acodec', 'pcm_s16le',  # Audio codec
            '-ar', '44100',  # Audio sample rate
            '-ac', '2',  # Number of audio channels
            wav_file
        ]
        # Run the ffmpeg command
        subprocess.run(command, check=True)
        # Remove the .part file after conversion
        os.remove(part_file)

    await asyncio.to_thread(convert_part_to_wav)

    # Function to convert audio if necessary
    def convert_wav_to_wav(input_file: str, output_file: str):
        print("Converting audio pt2...")
        input_file = TEMP_A2
        output_file = CONVERTED_AUDIO_FILE
        subprocess.run([
            'ffmpeg', '-i', input_file, 
            '-ar', '16000', '-ac', '1', '-f', 'wav', 
            output_file
        ], check=True)

    await asyncio.to_thread(convert_wav_to_wav)

    # Function to transcribe audio using Google Cloud Speech-to-Text
    def transcribe_audio(file_path: str):
        print("Transcribing audio...")
        file_path = CONVERTED_AUDIO_FILE
        model = whisper.load_model("base")  # Load Whisper model
        result = model.transcribe(file_path)  # Transcribe the audio file
        # json.dumps(result)
        print("Transcript: {}".format(result['text']))
    
    # while True:
    await convert_part_to_wav()
    asyncio.sleep(1)
    await convert_wav_to_wav
    asyncio.sleep(1)
    await transcribe_audio()
    asyncio.sleep(1)

threads = []

async def main():
    # Set up Google Cloud credentials
    # os.environ["GOOGLE_APPLICATION_CREDENTIALS"] = GOOGLE_APPLICATION_CREDENTIALS
    # threads = list()
    
    # for i in range(5):
    #     th = 
    # th1 = TimelimitedThread(target=download_audio, args=(LIVE_STREAM_URL, TEMP_AUDIO_FILE), time_limit=THREAD_TIME_LIMIT)
        # threads.append(th)
    # download_audio(LIVE_STREAM_URL, TEMP_AUDIO_FILE)
    # th1 = threading.Thread(target=download_audio, args=(LIVE_STREAM_URL, TEMP_AUDIO_FILE))
    download_task = asyncio.create_task(download_audio_part(LIVE_STREAM_URL, TEMP_AUDIO_FILE))
    
    # try:
    #     await asyncio.wait_for(download, timeout=60)
    # except asyncio.TimeoutError:
    #     print("Download task killed")
    #     download.cancel()
    # convert_task = asyncio.create_task(convert(PART_FILE, TEMP_AUDIO_FILE))

    await download_task

    
    
    # os.remove(TEMP_A2)
    # os.remove(CONVERTED_AUDIO_FILE)
    print("yo")
      

if __name__ == "__main__":
    asyncio.run(main())