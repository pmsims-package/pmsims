## Docker image for pmsims development

1. Install Docker
2. Build the image

   ```
   docker build -t pmsims .
   ```
3. Run the image from the `pmsims` directory root

   ```
   docker run --rm -ti -v $(pwd):/usr/local/src r-dev
   ```
