import yt_dlp
import subprocess
import os
import whisper
from google.cloud import speech_v1 as speech
import json
# import sys
import threading
import time
# import threading
# import time

# Configure paths and URLs
LIVE_STREAM_URL = 'https://www.youtube.com/watch?v=Nn93IFikHGM'  # Replace with the actual URL
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


def download_audio_part(url: str, output_file: str):
    print("Downloading audio...")
    # Ensure the output file has a .part extension
    part_file = f"{output_file}.part"
    
    ydl_opts = {
        'format': 'bestaudio/best',
        'outtmpl': part_file,
        'noplaylist': True,
        'quiet': True,
        'username': 'oauth2',
        'password': '',
    }
    
    with yt_dlp.YoutubeDL(ydl_opts) as ydl:
        ydl.download([url])

def convert_part_to_wav(part_file: str, output_file: str):
    print("Converting audio pt1...")
    part_file = PART_FILE
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

    time.sleep(1) # may want to lower later

    # Function to convert audio if necessary
    # def convert_audio(input_file: str, output_file: str):
    print("Converting audio pt2...")
    input_file = TEMP_A2
    output_file = CONVERTED_AUDIO_FILE
    subprocess.run([
        'ffmpeg', '-i', input_file, 
        '-ar', '16000', '-ac', '1', '-f', 'wav', 
        output_file
    ], check=True)

    time.sleep(1) # was 10

    # Function to transcribe audio using Google Cloud Speech-to-Text
    # def transcribe_audio(file_path: str):
    print("Transcribing audio...")
    file_path = CONVERTED_AUDIO_FILE
    model = whisper.load_model("base")  # Load Whisper model
    result = model.transcribe(file_path)  # Transcribe the audio file
    # json.dumps(result)
    print("Transcript: {}".format(result['text']))

threads = []

def main():
    # Set up Google Cloud credentials
    # os.environ["GOOGLE_APPLICATION_CREDENTIALS"] = GOOGLE_APPLICATION_CREDENTIALS
    threads = list()
    
    # for i in range(5):
    #     th = 
    # th1 = TimelimitedThread(target=download_audio, args=(LIVE_STREAM_URL, TEMP_AUDIO_FILE), time_limit=THREAD_TIME_LIMIT)
        # threads.append(th)
    # download_audio(LIVE_STREAM_URL, TEMP_AUDIO_FILE)
    # th1 = threading.Thread(target=download_audio, args=(LIVE_STREAM_URL, TEMP_AUDIO_FILE))
    th1 = threading.Thread(target=download_audio_part, args=(LIVE_STREAM_URL, TEMP_AUDIO_FILE))
    th1.start()
    threads.append(th1)
    # for th in threads:
    # th.start()
    # th.join()
    seconds = 5
    print(f"Waiting {seconds} secs")
    # for th in threads:
    time.sleep(seconds)
    while True:
        th2 = threading.Thread(target=convert_part_to_wav, args=(PART_FILE, TEMP_AUDIO_FILE))
        th2.start()
        threads.append(th2)

        os.remove(TEMP_A2)
        os.remove(CONVERTED_AUDIO_FILE)
        print("yo")
        for th in threads:
            th.join()
        

if __name__ == "__main__":
    main()
    # download_audio_part(LIVE_STREAM_URL, TEMP_AUDIO_FILE)