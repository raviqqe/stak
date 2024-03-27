export const notify = async (message: string): Promise<void> => {
  if (Notification.permission === "denied") {
    return;
  } else if (Notification.permission !== "granted") {
    await Notification.requestPermission();
  }

  new Notification(message);
};
